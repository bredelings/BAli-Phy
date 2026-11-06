#!/usr/bin/env python3

import argparse
import getpass
import os
import pathlib
import re
import subprocess
import sys
import tempfile
import time


# Wine compatibility: HOME selects the isolated prefix but is not passed to the
# guest. Remove this mapping if the test can inject the cache root explicitly.
def test_cache_root(home, wrapper):
    if wrapper:
        if pathlib.Path(wrapper[0]).name not in {"wine", "wine64"}:
            raise ValueError(f"cannot locate cache for wrapper {wrapper!r}")
        data_root = (home / ".wine" / "drive_c" / "users" / getpass.getuser() /
                     "AppData" / "Local")
    else:
        data_root = home / ".local" / "share"
    return data_root / "bali-phy" / "cache"


# Run the fixture with an isolated user cache and return its complete log.
def run_program(wrapper, executable, fixture, package_path, home, extra_args=(), command=("run", "Main")):
    env = os.environ.copy()
    env["HOME"] = str(home)
    command = wrapper + [executable, "-V", package_path, *extra_args, *command]
    result = subprocess.run(command, cwd=fixture, env=env,
                            encoding="utf-8", capture_output=True)
    if result.returncode != 0:
        raise RuntimeError(result.stdout + result.stderr)
    return result.stdout + result.stderr


# Load Main through test-module and require --dump-ffi to read the same
# durable metadata from the warm compiled-module cache.
def run_ffi_dump(wrapper, executable, fixture, package_path, home):
    env = os.environ.copy()
    env["HOME"] = str(home)
    command = wrapper + [executable, "--dump-ffi", "-V", package_path,
                         "test-module", "Main.hs"]
    result = subprocess.run(command, cwd=fixture, env=env,
                            encoding="utf-8", capture_output=True)
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
        cache_root = test_cache_root(home, args.wrapper)
        cache_root.mkdir(parents=True)

        fake_caches = []
        recent_time = time.time()
        for number in range(1, 5):
            cache = cache_root / f"{number:016x}"
            cache.mkdir()
            marker = cache / ".last-used"
            marker.touch()
            timestamp = recent_time - 60 * (5 - number)
            os.utime(marker, (timestamp, timestamp))
            fake_caches.append(cache)

        removable_legacy = cache_root / "Compiler" / "Internal"
        removable_legacy.mkdir(parents=True)
        (removable_legacy / "Old.hs.mod").write_bytes(b"legacy")

        retained_legacy = cache_root / "Data"
        retained_legacy.mkdir()
        (retained_legacy / "Old.hs.mod").write_bytes(b"legacy")
        (retained_legacy / "keep.txt").write_text("unrelated\n", encoding="utf-8")

        first_log = run_program(args.wrapper, args.executable, args.fixture,
                                args.package_path, home)
        caches = managed_caches(cache_root)
        if len(caches) != 4:
            raise AssertionError(f"expected four managed caches, found {caches}")
        if fake_caches[0].exists():
            raise AssertionError("the oldest executable cache was not removed")

        fake_cache_names = {path.name for path in fake_caches}
        current_candidates = [path for path in caches if path.name not in fake_cache_names]
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
        stale_time = time.time() - 60
        os.utime(marker, (stale_time, stale_time))
        second_log = run_program(args.wrapper, args.executable, args.fixture,
                                 args.package_path, home)
        if marker.stat().st_mtime <= stale_time:
            raise AssertionError("the current cache marker was not touched")
        if "[ Loading Main ]" not in second_log:
            raise AssertionError("the warm run did not load Main")
        if "[ Compiling Main ]" in second_log:
            raise AssertionError("the warm run recompiled Main")

        ffi_log = run_ffi_dump(args.wrapper, args.executable, args.fixture,
                               args.package_path, home)
        if "[ Loading Main ]" not in ffi_log:
            raise AssertionError("--dump-ffi did not load Main from cache")
        if "[ Compiling Main ]" in ffi_log:
            raise AssertionError("--dump-ffi unexpectedly recompiled Main")
        for expected in [
                "cachedSubstring$raw",
                "Vector:builtin_function_cppSubString",
                "String -> Int -> Int -> String",
                "CPPString -> Int -> Int -> CPPString",
                "slot 0: CPPString",
                "slot 1: Int",
                "slot 2: Int",
                "raw: CPPString"]:
            if expected not in ffi_log:
                raise AssertionError(
                    f"cached --dump-ffi report omitted {expected!r}:\n{ffi_log}")

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

        # Cached dependency discovery must preserve source invalidation and parser diagnostics.
        # Execution-only tests miss warm warning loss and stale import lists; retain these checks
        # while dependencies can be discovered without parsing source.
        sources = home / "sources"
        sources.mkdir()
        (sources / "Main.hs").write_text("import Warn\nmain = print value\n", encoding="utf-8")
        warning_source = (
            "{-# LANGUAGE CPP, NoImplicitPrelude #-}\n{-# UNKNOWN #-}\nmodule Warn where\n"
            "#if SELECT_OTHER\nimport Other\n#else\nimport Choice\n#endif\n"
            "value = choice\n")
        (sources / "Warn.hs").write_text(warning_source, encoding="utf-8")
        (sources / "Choice.hs").write_text(
            "{-# LANGUAGE NoImplicitPrelude #-}\nmodule Choice where\nimport Compiler.Num\nchoice = 1 :: Int\n",
            encoding="utf-8")
        (sources / "Other.hs").write_text(
            "{-# LANGUAGE NoImplicitPrelude #-}\nmodule Other where\nimport Compiler.Num\nchoice = 3 :: Int\n",
            encoding="utf-8")

        # Exercise the same dependency with and without an AST, including a body-cache failure
        # caused by a changed dependency after the cached parser warning has already been replayed.
        for stage in ("cold", "warm", "corrupt", "dependency-changed", "cpp-changed", "dump", "forced"):
            extra = []
            if stage == "corrupt":
                artifact = current / "Warn.hs.mod"
                artifact.write_bytes(artifact.read_bytes()[:-1])
            if stage == "dependency-changed":
                source = sources / "Choice.hs"
                timestamp = source.stat().st_mtime_ns
                source.write_text(
                    "{-# LANGUAGE NoImplicitPrelude #-}\nmodule Choice where\nimport Compiler.Num\nchoice = 2 :: Int\n",
                    encoding="utf-8")
                os.utime(source, ns=(timestamp, timestamp))
            if stage in {"cpp-changed", "dump", "forced"}:
                extra += ["-DSELECT_OTHER=1"]
            command = ("run", "Main")
            if stage == "dump":
                # Limit the parsed dump to the small imported chain, rather than all of Prelude.
                (sources / "DumpRoot.hs").write_text(
                    "{-# LANGUAGE NoImplicitPrelude #-}\nmodule DumpRoot where\nimport Warn\nf = value\n",
                    encoding="utf-8")
                command = ("test-module", "DumpRoot.hs")
                extra += ["--dump-parsed"]
            if stage == "forced":
                extra += ["--recompile=Warn"]
            log = run_program(args.wrapper, args.executable, sources, args.package_path, home, extra, command)
            if log.count("Unknown pragma") != 1 or "Warn.hs:2" not in log:
                raise AssertionError(f"{stage}: parser warning missing, duplicated, or misplaced:\n{log}")
            expected = {"cold": "1", "warm": "1", "corrupt": "1", "dependency-changed": "2",
                        "cpp-changed": "3", "dump": "3", "forced": "3"}[stage]
            if stage != "dump" and expected not in log.splitlines():
                raise AssertionError(f"{stage}: expected result {expected}:\n{log}")
            action = "Loading" if stage in {"warm", "dump"} else "Compiling"
            if f"[ {action} Warn ]" not in log:
                raise AssertionError(f"{stage}: expected {action} Warn:\n{log}")
            if stage == "dump" and "module Warn" not in log:
                raise AssertionError(f"parsed dump omitted the cached dependency:\n{log}")

        # Plain source import edits must also replace the cached dependency list.
        (sources / "Warn.hs").write_text(
            "{-# UNKNOWN #-}\nmodule Warn where\nimport Choice\nvalue = choice\n", encoding="utf-8")
        log = run_program(args.wrapper, args.executable, sources, args.package_path, home)
        if "2" not in log.splitlines() or "[ Compiling Warn ]" not in log:
            raise AssertionError(f"source import edit did not invalidate the cached prefix:\n{log}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
