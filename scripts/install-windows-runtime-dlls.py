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

WINDOWS_SYSTEM_DLLS = {
    "advapi32.dll",
    "dwrite.dll",
    "gdi32.dll",
    "kernel32.dll",
    "msimg32.dll",
    "msvcrt.dll",
    "ole32.dll",
    "rpcrt4.dll",
    "shell32.dll",
    "user32.dll",
    "usp10.dll",
    "ws2_32.dll",
}


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

    output = result.stdout.strip()
    if not output:
        return None

    path = pathlib.Path(output)
    if path.name == dll_name and path.exists():
        return path
    if path.name == dll_name and str(path) == dll_name:
        return None
    if path.is_file():
        return path
    return None


def candidate_runtime_dirs(cxx):
    dirs = []

    for command in cxx:
        command_path = pathlib.Path(command)
        if command_path.parent != pathlib.Path(".") and command_path.parent.is_dir():
            dirs.append(command_path.parent)

        resolved = shutil.which(command)
        if resolved:
            dirs.append(pathlib.Path(resolved).parent)

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


def find_runtime_dll(cxx, dll_name):
    dll_path = gcc_print_file_name(cxx, dll_name)
    if dll_path:
        return dll_path

    for directory in candidate_runtime_dirs(cxx):
        dll_path = directory / dll_name
        if dll_path.exists():
            return dll_path

    return None


def install_gcc_runtime_dlls(cxx, bindir):
    for dll_group in GCC_RUNTIME_GROUPS:
        for dll_name in dll_group:
            dll_path = find_runtime_dll(cxx, dll_name)
            if dll_path:
                copy_dll(dll_path, bindir)
                break
        else:
            raise RuntimeError(f"could not find any of: {', '.join(dll_group)}")

    for dll_name in OPTIONAL_GCC_RUNTIME_DLLS:
        dll_path = find_runtime_dll(cxx, dll_name)
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


# Copy data directories whose locations are resolved relative to the installed
# MSYS2 runtime, while preserving any files Meson has already installed.
def install_tree(source, target):
    if not source.is_dir():
        return
    shutil.copytree(source, target, dirs_exist_ok=True)
    log(f"installed {source} -> {target}")


# Convert MSYS2's Unix-style MINGW_PREFIX for native Windows Python. This lets
# native MSYS2 installs use the same dependency closure as cross-built packages.
def native_msys2_prefix():
    mingw_prefix = os.environ.get("MINGW_PREFIX")
    if os.name != "nt" or not mingw_prefix:
        return None

    prefix = pathlib.Path(mingw_prefix)
    if prefix.is_absolute() and prefix.is_dir():
        return prefix

    cygpath = shutil.which("cygpath")
    if not cygpath:
        raise RuntimeError("MINGW_PREFIX is set, but cygpath could not be found")
    try:
        result = subprocess.run(
            [cygpath, "-w", mingw_prefix],
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
    except subprocess.CalledProcessError as error:
        raise RuntimeError(
            f"failed to convert MINGW_PREFIX={mingw_prefix}: {error.stderr.strip()}"
        ) from error

    prefix = pathlib.Path(result.stdout.strip())
    if not prefix.is_dir():
        raise RuntimeError(f"MINGW_PREFIX directory does not exist: {prefix}")
    return prefix


# Index the controlled runtime case-insensitively, matching Windows DLL lookup
# while rejecting an ambiguous sysroot rather than choosing arbitrarily.
def available_runtime_dlls(source_bin):
    dlls = {}
    for dll in source_bin.iterdir():
        if not dll.is_file() or dll.suffix.lower() != ".dll":
            continue
        key = dll.name.casefold()
        if key in dlls:
            raise RuntimeError(f"duplicate DLL names in {source_bin}: {dlls[key]}, {dll}")
        dlls[key] = dll
    if not dlls:
        raise RuntimeError(f"no DLLs found in {source_bin}")
    return dlls


# Existing runtime DLLs in bin may come from an earlier incremental install;
# exclude them as roots so they cannot keep otherwise-unused dependencies alive.
def installed_pe_roots(prefix, bindir, runtime_dlls):
    roots = []
    for path in prefix.rglob("*"):
        if not path.is_file():
            continue
        suffix = path.suffix.lower()
        if suffix == ".exe":
            roots.append(path)
        elif suffix == ".dll":
            is_runtime_dll = (
                path.parent == bindir and path.name.casefold() in runtime_dlls
            )
            if not is_runtime_dll:
                roots.append(path)
    if not roots:
        raise RuntimeError(f"no installed Windows executables or libraries found in {prefix}")
    return roots


# Read the PE import table with the build-machine objdump, requiring every
# import to come from the package, the controlled runtime, or Windows itself.
def imported_runtime_dlls(objdump, object_path, runtime_dlls, project_files):
    try:
        result = subprocess.run(
            [objdump, "-p", object_path],
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
    except subprocess.CalledProcessError as error:
        raise RuntimeError(
            f"failed to inspect {object_path} with {objdump}: {error.stderr.strip()}"
        ) from error

    imports = []
    marker = "DLL Name:"
    for line in result.stdout.splitlines():
        line = line.strip()
        if line.startswith(marker):
            dll_name = line[len(marker) :].strip()
            key = dll_name.casefold()
            dll = runtime_dlls.get(key)
            if dll:
                imports.append(dll)
            elif (
                key not in project_files
                and key not in WINDOWS_SYSTEM_DLLS
                and not key.startswith(("api-ms-win-", "ext-ms-win-"))
            ):
                raise RuntimeError(
                    f"{object_path} imports {dll_name}, which is absent from "
                    "the installation, MSYS2 runtime, and Windows system DLL list"
                )
    return imports


# Traverse imports from installed project binaries to find the recursively
# required part of the coherent MSYS2 runtime.
def runtime_dll_closure(objdump, roots, runtime_dlls):
    needed = {}
    scanned = set()
    queue = list(roots)
    project_files = {path.name.casefold() for path in roots}
    while queue:
        object_path = queue.pop()
        object_key = str(object_path.resolve())
        if object_key in scanned:
            continue
        scanned.add(object_key)

        for dll in imported_runtime_dlls(
            objdump, object_path, runtime_dlls, project_files
        ):
            key = dll.name.casefold()
            if key not in needed:
                needed[key] = dll
                queue.append(dll)
    if not needed:
        raise RuntimeError(
            "installed Windows programs have no dependencies in the MSYS2 runtime"
        )
    return sorted(needed.values(), key=lambda path: path.name.casefold())


# Use the MSYS2 versions of all imported libraries, including its GCC runtime;
# replacing them with an older cross-compiler runtime can remove required symbols.
def install_mingw_runtime(mingw_prefix, objdump, prefix, bindir):
    mingw_prefix = pathlib.Path(mingw_prefix)
    source_bin = mingw_prefix / "bin"
    runtime_dlls = available_runtime_dlls(source_bin)
    roots = installed_pe_roots(prefix, bindir, runtime_dlls)
    dlls = runtime_dll_closure(objdump, roots, runtime_dlls)

    # An incremental install may contain runtime DLLs required by an older build. Remove recognized
    # runtime files outside the new closure, but never remove files from the MSYS2 source itself.
    needed = {dll.name.casefold() for dll in dlls}
    for target in bindir.iterdir():
        source = runtime_dlls.get(target.name.casefold())
        if source is None or target.name.casefold() in needed or not target.is_file():
            continue
        if target.resolve() == source.resolve():
            continue
        target.unlink()
        log(f"removed obsolete runtime DLL {target}")

    for dll in dlls:
        copy_dll(dll, bindir)
    log(f"installed recursive runtime closure ({len(dlls)} DLLs)")

    install_tree(mingw_prefix / "etc" / "fonts", prefix / "etc" / "fonts")
    install_tree(
        mingw_prefix / "share" / "fontconfig", prefix / "share" / "fontconfig"
    )
    install_tree(mingw_prefix / "share" / "licenses", prefix / "share" / "licenses")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--bindir", required=True)
    parser.add_argument("--boost-root", action="append", default=[])
    parser.add_argument("--mingw-prefix")
    parser.add_argument("--objdump")
    parser.add_argument("--cxx", nargs=argparse.REMAINDER, required=True)
    args = parser.parse_args()

    prefix = install_prefix()
    bindir = prefix / args.bindir
    mingw_prefix = args.mingw_prefix or native_msys2_prefix()
    objdump = args.objdump
    if mingw_prefix and not objdump:
        objdump = shutil.which("objdump")

    if mingw_prefix:
        if not objdump:
            raise RuntimeError("objdump is required to resolve Windows runtime DLLs")
        install_mingw_runtime(mingw_prefix, objdump, prefix, bindir)
    else:
        if not args.cxx:
            raise RuntimeError("--cxx requires at least one command argument")
        install_gcc_runtime_dlls(args.cxx, bindir)
        install_boost_dlls(args.boost_root, bindir)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as error:
        print(f"install-windows-runtime-dlls.py: error: {error}", file=sys.stderr)
        sys.exit(1)
