#!/usr/bin/env python3
"""Obtain xslTNG and MathJax; use normally installed Python tools and fonts."""

from pathlib import Path
import shutil
import subprocess
import sys
import urllib.request
import zipfile


# Supply a convenient xslTNG download when absent, while accepting an existing installation.
# npm resolves compatible MathJax releases; setup leaves Python environments and fonts alone.
def main():
    if len(sys.argv) != 3:
        raise SystemExit('Usage: python3 setup-guide.py TOOLS_DIR DOCBOOK_XSLTNG')
    source = Path(__file__).resolve().parent
    tools, xsltng = (Path(arg).resolve() for arg in sys.argv[1:])
    tools.mkdir(parents=True, exist_ok=True)
    if not (xsltng / 'bin' / 'docbook').is_file():
        if xsltng != tools / 'docbook-xslTNG-2.8.4':
            raise SystemExit(f'No xslTNG installation at {xsltng}; check DOCBOOK_XSLTNG.')
        archive = tools / 'docbook-xslTNG-2.8.4.zip'
        if not archive.exists():
            url = f'https://codeberg.org/docbook/xslTNG/releases/download/2.8.4/{archive.name}'
            temporary = archive.with_suffix('.download')
            with urllib.request.urlopen(url, timeout=60) as response, temporary.open('wb') as output:
                shutil.copyfileobj(response, output)
            temporary.replace(archive)
        with zipfile.ZipFile(archive) as bundle:
            bundle.extractall(tools)
        (xsltng / 'bin' / 'docbook').chmod(0o755)
    npm_dir = tools / 'npm'
    npm_dir.mkdir(exist_ok=True)
    shutil.copyfile(source / 'package.json', npm_dir / 'package.json')
    subprocess.run(['npm', 'install', '--prefix', str(npm_dir), '--package-lock=false',
                    '--ignore-scripts', '--no-audit', '--no-fund'], check=True)


if __name__ == '__main__':
    main()
