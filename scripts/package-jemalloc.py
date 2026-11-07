#!/usr/bin/env python3
"""Bundle jemalloc for Unix release archives, or check a relocated package."""

import argparse
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys


# Read the library selected by the executable, rather than assuming a package-manager prefix.
def linked_jemalloc(binary):
    command = ['otool', '-L'] if sys.platform == 'darwin' else ['ldd']
    output = subprocess.check_output(command + [str(binary)], text=True)
    if sys.platform == 'darwin':
        return next((line.strip().split(' (', 1)[0] for line in output.splitlines()[1:]
                     if 'libjemalloc' in line), None)
    match = re.search(r'\blibjemalloc[^\s]*\s+=>\s+(.+?)\s+\(', output)
    if re.search(r'\blibjemalloc\S*\s+=>\s+not found', output):
        raise RuntimeError(f'jemalloc cannot be resolved for {binary}')
    return match[1] if match else None


# Inspect only Mach-O files: scripts and data files also live in these directories.
def macho_files(prefix):
    magic = {b'\xcf\xfa\xed\xfe', b'\xce\xfa\xed\xfe', b'\xfe\xed\xfa\xcf',
             b'\xfe\xed\xfa\xce', b'\xca\xfe\xba\xbe', b'\xca\xfe\xba\xbf'}
    for directory in [prefix / 'bin', prefix / 'lib/bali-phy']:
        for path in directory.iterdir():
            if path.is_file() and not path.is_symlink():
                with path.open('rb') as stream:
                    if stream.read(4) in magic:
                        yield path


# Rewrite every direct reference before the workflow signs the resulting Mach-O files.
def bundle(prefix, library, license_path):
    destination = prefix / 'lib/bali-phy' / library.name
    shutil.copy2(library, destination)
    if sys.platform == 'darwin':
        destination.chmod(destination.stat().st_mode | 0o200)
        subprocess.run(['install_name_tool', '-id', '@rpath/' + library.name,
                        str(destination)], check=True)
        for binary in macho_files(prefix):
            if binary == destination:
                continue
            reference = linked_jemalloc(binary)
            if reference:
                relative = os.path.relpath(destination, binary.parent)
                binary.chmod(binary.stat().st_mode | 0o200)
                subprocess.run(['install_name_tool', '-change', reference,
                                '@loader_path/' + relative, str(binary)], check=True)
    license_dir = prefix / 'share/licenses/jemalloc'
    license_dir.mkdir(parents=True, exist_ok=True)
    shutil.copy2(license_path, license_dir / 'COPYING')


# Exercise the loader and allocator in the relocated executable, without search-path overrides.
def check(prefix):
    binary = prefix / 'bin/bali-phy'
    if not (prefix / 'share/licenses/jemalloc/COPYING').is_file():
        raise RuntimeError('Packaged jemalloc license is missing')
    environment = os.environ.copy()
    for key in ['LD_PRELOAD', 'LD_LIBRARY_PATH', 'DYLD_INSERT_LIBRARIES', 'DYLD_LIBRARY_PATH',
                'DYLD_FALLBACK_LIBRARY_PATH']:
        environment.pop(key, None)
    environment['MALLOC_CONF'] = 'stats_print:true'
    if sys.platform == 'darwin':
        environment['DYLD_PRINT_LIBRARIES'] = '1'
    result = subprocess.run([str(binary), '--version'], env=environment,
                            text=True, capture_output=True, check=True)
    if 'Begin jemalloc statistics' not in result.stderr:
        raise RuntimeError('Packaged executable did not report jemalloc statistics')
    if sys.platform == 'darwin':
        loaded = re.findall(r'(/.*libjemalloc[^/\n]*\.dylib)', result.stderr)
        if not loaded or any(Path(path).resolve().parent != (prefix / 'lib/bali-phy').resolve()
                             for path in loaded):
            raise RuntimeError(f'jemalloc was not loaded from the package: {loaded}')
        for binary in macho_files(prefix):
            subprocess.run(['codesign', '--verify', str(binary)], check=True)
    else:
        # ldd must use the same clean search environment as the executable.
        output = subprocess.check_output(['ldd', str(binary)], env=environment, text=True)
        match = re.search(r'\blibjemalloc\S*\s+=>\s+(.+?)\s+\(', output)
        if not match or Path(match[1]).resolve().parent != (prefix / 'lib/bali-phy').resolve():
            raise RuntimeError('jemalloc was not loaded from the package')
    print('Verified packaged jemalloc:', prefix)


# Ordinary CI may use the system allocator; release jobs require jemalloc explicitly.
def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('prefix', type=Path)
    parser.add_argument('--license', type=Path)
    parser.add_argument('--required', action='store_true')
    parser.add_argument('--check', action='store_true')
    args = parser.parse_args()
    prefix = args.prefix.resolve()
    if args.check:
        check(prefix)
        return
    reference = linked_jemalloc(prefix / 'bin/bali-phy')
    if not reference:
        if args.required:
            raise RuntimeError('Release executable is not linked to jemalloc')
        print('System allocator selected; no jemalloc to package')
        return
    library = Path(reference)
    if not library.is_absolute() or not library.is_file():
        raise RuntimeError(f'Expected an installed jemalloc library, got {reference}')
    if args.license is None or not args.license.is_file():
        raise RuntimeError('Supply the license file for the packaged jemalloc version')
    bundle(prefix, library, args.license)


if __name__ == '__main__':
    main()
