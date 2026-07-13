#!/usr/bin/env python3

import argparse
import os
import pathlib
import re
import subprocess
import sys
import tempfile


# Run the fixture with an isolated user cache and return its complete log.
def run_program(wrapper, executable, fixture, package_path, home):
    env = os.environ.copy()
    env["HOME"] = str(home)
    command = wrapper + [executable, "run", "Main", "-V", package_path]
    result = subprocess.run(command, cwd=fixture, env=env,
                            text=True, capture_output=True)
    if result.returncode != 0:
        raise RuntimeError(result.stdout + result.stderr)
    return result.stdout + result.stderr


# Return only executable-keyed cache directories managed by the loader.
def managed_caches(cache_root):
    cache_key = re.compile(r"^[0-9a-f]{16}$")
    return sorted(path for path in cache_root.iterdir()
                  if path.is_dir() and cache_key.fullmatch(path.name) and
                  (path / ".last-used").is_file())


# Check migration, retention, compression, warm loads, and corruption recovery.
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--wrapper", action="append", default=[])
    parser.add_argument("executable")
    parser.add_argument("fixture")
    parser.add_argument("package_path")
    args = parser.parse_args()

    with tempfile.TemporaryDirectory(prefix="bali-phy-module-cache-") as tmp:
        home = pathlib.Path(tmp)
        cache_root = home / ".local" / "share" / "bali-phy" / "cache"
        cache_root.mkdir(parents=True)

        fake_caches = []
        for number in range(1, 5):
            cache = cache_root / f"{number:016x}"
            cache.mkdir()
            marker = cache / ".last-used"
            marker.touch()
            os.utime(marker, (number, number))
            fake_caches.append(cache)

        removable_legacy = cache_root / "Compiler" / "Internal"
        removable_legacy.mkdir(parents=True)
        (removable_legacy / "Old.hs.mod").write_bytes(b"legacy")

        retained_legacy = cache_root / "Data"
        retained_legacy.mkdir()
        (retained_legacy / "Old.hs.mod").write_bytes(b"legacy")
        (retained_legacy / "keep.txt").write_text("unrelated\n")

        first_log = run_program(args.wrapper, args.executable, args.fixture,
                                args.package_path, home)
        caches = managed_caches(cache_root)
        if len(caches) != 4:
            raise AssertionError(f"expected four managed caches, found {caches}")
        if fake_caches[0].exists():
            raise AssertionError("the oldest executable cache was not removed")

        current_candidates = [path for path in caches if path not in fake_caches]
        if len(current_candidates) != 1:
            raise AssertionError(f"could not identify current cache: {caches}")
        current = current_candidates[0]
        main_module = current / "Main.hs.mod"
        if not main_module.is_file():
            raise AssertionError("the first run did not cache Main")
        if not main_module.read_bytes().startswith(b"\x28\xb5\x2f\xfd"):
            raise AssertionError("the cached module is not a zstd frame")
        if "[ Compiling Main ]" not in first_log:
            raise AssertionError("the cold run did not compile Main")

        if (removable_legacy / "Old.hs.mod").exists():
            raise AssertionError("legacy module artifact was not removed")
        if (cache_root / "Compiler").exists():
            raise AssertionError("empty legacy directories were not removed")
        if (retained_legacy / "Old.hs.mod").exists():
            raise AssertionError("legacy module beside unrelated data was not removed")
        if not (retained_legacy / "keep.txt").is_file():
            raise AssertionError("unrelated legacy cache data was removed")

        marker = current / ".last-used"
        os.utime(marker, (1, 1))
        second_log = run_program(args.wrapper, args.executable, args.fixture,
                                 args.package_path, home)
        if marker.stat().st_mtime <= 1:
            raise AssertionError("the current cache marker was not touched")
        if "[ Loading Main ]" not in second_log:
            raise AssertionError("the warm run did not load Main")
        if "[ Compiling Main ]" in second_log:
            raise AssertionError("the warm run recompiled Main")

        compressed_module = main_module.read_bytes()
        main_module.write_bytes(compressed_module[:-1])
        third_log = run_program(args.wrapper, args.executable, args.fixture,
                                args.package_path, home)
        if "[ Compiling Main ]" not in third_log:
            raise AssertionError("a corrupt cached module was not recompiled")
        if not main_module.read_bytes().startswith(b"\x28\xb5\x2f\xfd"):
            raise AssertionError("recompilation did not replace the corrupt module")

        fourth_log = run_program(args.wrapper, args.executable, args.fixture,
                                 args.package_path, home)
        if "[ Loading Main ]" not in fourth_log:
            raise AssertionError("the repaired cached module was not loaded")
        if "[ Compiling Main ]" in fourth_log:
            raise AssertionError("the repaired cached module was recompiled")

    return 0


if __name__ == "__main__":
    sys.exit(main())
