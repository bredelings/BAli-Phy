#!/usr/bin/env python3

import argparse
import os
import pathlib
import shutil
import subprocess
import sys


GCC_RUNTIME_GROUPS = [
    ["libgcc_s_seh-1.dll", "libgcc_s_dw2-1.dll", "libgcc_s_sjlj-1.dll"],
    ["libstdc++-6.dll"],
    ["libwinpthread-1.dll"],
]

OPTIONAL_GCC_RUNTIME_DLLS = [
    "libssp-0.dll",
]


def log(message):
    print(f"install-windows-runtime-dlls.py: {message}")


def install_prefix():
    prefix = os.environ.get("MESON_INSTALL_DESTDIR_PREFIX")
    if not prefix:
        prefix = os.environ.get("MESON_INSTALL_PREFIX")
    if not prefix:
        raise RuntimeError("MESON_INSTALL_DESTDIR_PREFIX and MESON_INSTALL_PREFIX are both unset")
    return pathlib.Path(prefix)


def copy_dll(source, bindir):
    source = pathlib.Path(source)
    if not source.exists():
        raise RuntimeError(f"DLL does not exist: {source}")

    target = bindir / source.name
    if source.resolve() == target.resolve():
        return

    bindir.mkdir(parents=True, exist_ok=True)
    shutil.copy2(source, target)
    log(f"installed {source} -> {target}")


def gcc_print_file_name(cxx, dll_name):
    try:
        result = subprocess.run(
            cxx + ["--print-file-name", dll_name],
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
    except subprocess.CalledProcessError as error:
        raise RuntimeError(
            f"failed to locate {dll_name} with {' '.join(cxx)}: {error.stderr.strip()}"
        ) from error

    path = pathlib.Path(result.stdout.strip())
    if path.name == dll_name and path.exists():
        return path
    if path.name == dll_name and str(path) == dll_name:
        return None
    if path.exists():
        return path
    return None


def install_gcc_runtime_dlls(cxx, bindir):
    for dll_group in GCC_RUNTIME_GROUPS:
        for dll_name in dll_group:
            dll_path = gcc_print_file_name(cxx, dll_name)
            if dll_path:
                copy_dll(dll_path, bindir)
                break
        else:
            raise RuntimeError(f"could not find any of: {', '.join(dll_group)}")

    for dll_name in OPTIONAL_GCC_RUNTIME_DLLS:
        dll_path = gcc_print_file_name(cxx, dll_name)
        if dll_path:
            copy_dll(dll_path, bindir)


def path_entries():
    path = os.environ.get("PATH", "")
    if not path:
        return []
    return [pathlib.Path(entry) for entry in path.split(os.pathsep) if entry]


def boost_candidate_dirs(roots):
    dirs = []

    for root in roots:
        if root:
            root_path = pathlib.Path(root)
            dirs += [root_path / "bin", root_path / "lib", root_path]

    for env_var in ["BOOST_ROOT", "BOOSTROOT"]:
        root = os.environ.get(env_var)
        if root:
            root_path = pathlib.Path(root)
            dirs += [root_path / "bin", root_path / "lib", root_path]

    dirs += path_entries()

    seen = set()
    result = []
    for directory in dirs:
        key = str(directory)
        if key in seen:
            continue
        seen.add(key)
        if directory.is_dir():
            result.append(directory)
    return result


def install_boost_dlls(roots, bindir):
    copied = 0
    seen = set()
    for directory in boost_candidate_dirs(roots):
        for dll_path in sorted(directory.glob("libboost_*.dll")):
            key = dll_path.name
            if key in seen:
                continue
            seen.add(key)
            copy_dll(dll_path, bindir)
            copied += 1

    if copied == 0:
        log("warning: no Boost DLLs found")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--bindir", required=True)
    parser.add_argument("--boost-root", action="append", default=[])
    parser.add_argument("--cxx", nargs=argparse.REMAINDER, required=True)
    args = parser.parse_args()

    if not args.cxx:
        raise RuntimeError("--cxx requires at least one command argument")

    bindir = install_prefix() / args.bindir
    install_gcc_runtime_dlls(args.cxx, bindir)
    install_boost_dlls(args.boost_root, bindir)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as error:
        print(f"install-windows-runtime-dlls.py: error: {error}", file=sys.stderr)
        sys.exit(1)
