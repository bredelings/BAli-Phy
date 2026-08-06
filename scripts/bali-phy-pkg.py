#!/usr/bin/env python3

import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import re
import shutil
import sys
import tarfile
import tempfile
from urllib.error import HTTPError, URLError
from urllib.parse import urljoin, urlparse, urlunparse
from urllib.request import urlopen


PACKAGE_INDEX_URL = "https://www.bali-phy.org/packages/Packages"
COMMANDS = "install, install-archive, available, uninstall, info, packages, files, untracked, missing, installed, help"


class PackageManagerError(Exception):
    pass


# Select the same per-user data roots as the native BAli-Phy path lookup.
def get_user_data_dir(environ):
    home = environ.get("HOME")
    if home:
        return Path(home) / ".local" / "share" / "bali-phy"

    local_app_data = environ.get("LOCALAPPDATA")
    if local_app_data:
        return Path(local_app_data) / "bali-phy"

    raise PackageManagerError("Neither HOME nor LOCALAPPDATA is set.")


# Compare the dotted numeric versions used by BAli-Phy packages without an external dependency.
def parse_version(version):
    if not isinstance(version, str) or not re.fullmatch(r"[0-9]+(?:\.[0-9]+)*", version):
        raise PackageManagerError(f"Invalid package version '{version}'. Expected dotted non-negative integers.")

    components = [int(component) for component in version.split(".")]
    while len(components) > 1 and components[-1] == 0:
        components.pop()
    return tuple(components)


# Upgrade the old HTTP package metadata now that Python provides verified HTTPS support.
def secure_url(url):
    parsed = urlparse(url)
    if parsed.scheme == "http":
        parsed = parsed._replace(scheme="https")
        return urlunparse(parsed)
    if parsed.scheme in ("https", "file"):
        return url
    raise PackageManagerError(f"Unsupported package URL scheme in '{url}'.")


# Compute an archive digest incrementally so package size does not affect memory use.
def file_digest(filename, algorithm):
    digest = hashlib.new(algorithm)
    with Path(filename).open("rb") as input_file:
        for block in iter(lambda: input_file.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


class PackageManager:
    # Keep package contents and package-manager state under one explicit user data root.
    def __init__(self, user_dir, package_index_url=PACKAGE_INDEX_URL):
        self.user_dir = Path(user_dir)
        self.packages_dir = self.user_dir / "packages"
        self.info_dir = self.user_dir / "info"
        self.package_index_url = secure_url(package_index_url)

        self.packages_dir.mkdir(parents=True, exist_ok=True)
        self.info_dir.mkdir(parents=True, exist_ok=True)

    # Return package names in stable order, ignoring non-directory debris in the state directory.
    def installed_packages(self):
        return sorted(entry.name for entry in self.info_dir.iterdir() if entry.is_dir())

    # Treat the package information directory as the installed-state marker used by the old manager.
    def is_package_installed(self, name):
        return (self.info_dir / name).is_dir()

    # An installation marker or removal journal identifies work that must be cleaned up or resumed.
    def is_package_broken(self, name):
        package_dir = self.info_dir / name
        if not package_dir.is_dir():
            return False
        return (
            not (package_dir / "control.json").is_file()
            or not (package_dir / "Files").is_file()
            or (package_dir / "Installing").exists()
            or (package_dir / "FilesRemoved").exists()
        )

    # Load the control metadata retained for an installed package.
    def installed_package_info(self, name):
        filename = self.info_dir / name / "control.json"
        try:
            with filename.open(encoding="utf-8") as input_file:
                return json.load(input_file)
        except (OSError, json.JSONDecodeError) as error:
            raise PackageManagerError(f"Can't read '{filename}': {error}") from error

    # Return the version recorded in an installed package's control metadata.
    def package_version(self, name):
        info = self.installed_package_info(name)
        try:
            return info["Version"]
        except KeyError as error:
            raise PackageManagerError(f"Package '{name}' has no Version field.") from error

    # Read a package state file as newline-delimited package-relative paths.
    def _read_lines(self, filename):
        try:
            with Path(filename).open(encoding="utf-8") as input_file:
                return [line.rstrip("\r\n") for line in input_file]
        except OSError as error:
            raise PackageManagerError(f"Can't read '{filename}': {error}") from error

    # Return the manifest recorded when a package was installed.
    def installed_files_for_package(self, name):
        return self._read_lines(self.info_dir / name / "Files")

    # Combine the manifests of every installed package.
    def installed_files(self):
        return [
            filename
            for package in self.installed_packages()
            for filename in self.installed_files_for_package(package)
        ]

    # Convert a stored POSIX package path into a destination while preventing directory traversal.
    def _package_file(self, filename):
        if "\\" in filename:
            raise PackageManagerError(f"Invalid package path '{filename}'.")
        relative = PurePosixPath(filename)
        if (
            relative.is_absolute()
            or not relative.parts
            or any(part in ("", ".", "..") or ":" in part for part in relative.parts)
        ):
            raise PackageManagerError(f"Invalid package path '{filename}'.")
        return self.packages_dir.joinpath(*relative.parts)

    # List ordinary files currently present in the combined package directory.
    def present_files(self):
        files = []
        for filename in self.packages_dir.rglob("*"):
            if filename.is_file() or filename.is_symlink():
                files.append(filename.relative_to(self.packages_dir).as_posix())
        return sorted(files)

    # Find package-directory files which are not owned by any installed package.
    def untracked_files(self):
        installed = set(self.installed_files())
        return [filename for filename in self.present_files() if filename not in installed]

    # Find manifested package files which are absent from the combined package directory.
    def missing_files(self):
        present = set(self.present_files())
        return [filename for filename in self.installed_files() if filename not in present]

    # Validate a package name before using it as a package-state directory name.
    def _validate_package_name(self, name):
        if (
            not isinstance(name, str)
            or not name
            or name in (".", "..")
            or "/" in name
            or "\\" in name
            or ":" in name
        ):
            raise PackageManagerError(f"Invalid package name '{name}'.")

    # Read and validate an archive before changing an existing installation.
    def _archive_contents(self, archive):
        archive = Path(archive)
        if not archive.is_file():
            raise PackageManagerError(f"install: archive '{archive}' not found!\nAborting installation.")

        try:
            with tarfile.open(archive, "r:*") as package_archive:
                control_members = []
                files = []
                seen_files = set()

                for member in package_archive.getmembers():
                    if "\\" in member.name:
                        raise PackageManagerError(f"Archive contains invalid path '{member.name}'.")
                    member_path = PurePosixPath(member.name)
                    if member_path.is_absolute() or any(
                        part in ("", ".", "..") or ":" in part for part in member_path.parts
                    ):
                        raise PackageManagerError(f"Archive contains invalid path '{member.name}'.")

                    if member.name == "control.json":
                        if not member.isfile():
                            raise PackageManagerError("Archive entry 'control.json' is not a regular file.")
                        control_members.append(member)
                        continue

                    if not member_path.parts or member_path.parts[0] != "files":
                        continue
                    if len(member_path.parts) == 1 or member.isdir():
                        continue
                    if not member.isfile():
                        raise PackageManagerError(f"Archive package entry '{member.name}' is not a regular file.")

                    relative = PurePosixPath(*member_path.parts[1:])
                    relative_name = relative.as_posix()
                    if relative_name in seen_files:
                        raise PackageManagerError(f"Archive contains duplicate package file '{relative_name}'.")
                    seen_files.add(relative_name)
                    files.append((member.name, relative_name, member.mode))

                if len(control_members) != 1:
                    raise PackageManagerError("Archive must contain exactly one regular 'control.json' file.")
                control_file = package_archive.extractfile(control_members[0])
                if control_file is None:
                    raise PackageManagerError("Could not read 'control.json' from archive.")
                control_data = control_file.read()

            info = json.loads(control_data.decode("utf-8"))
        except PackageManagerError:
            raise
        except (OSError, tarfile.TarError, UnicodeDecodeError, json.JSONDecodeError) as error:
            raise PackageManagerError(f"Can't read package archive '{archive}': {error}") from error

        try:
            name = info["Package"]
            version = info["Version"]
        except (KeyError, TypeError) as error:
            raise PackageManagerError("Package control metadata must contain Package and Version fields.") from error
        self._validate_package_name(name)
        parse_version(version)
        return info, control_data, files

    # Reject package files which would overwrite another package or an untracked file.
    def _check_overwrite(self, files, ignore_package=None):
        archive_files = {filename for _, filename, _ in files}
        for package in self.installed_packages():
            if package == ignore_package:
                continue
            conflicts = archive_files.intersection(self.installed_files_for_package(package))
            if conflicts:
                conflict = sorted(conflicts)[0]
                raise PackageManagerError(
                    f"install: Package '{package}' contains conflicting file '{conflict}'.\nAborting installation."
                )

        conflicts = archive_files.intersection(self.untracked_files())
        if conflicts:
            conflict = sorted(conflicts)[0]
            raise PackageManagerError(
                f"install: Refusing to overwrite untracked file \"{conflict}\".\nAborting installation."
            )

    # Install a validated archive, retaining complete intended state if copying is interrupted.
    def install_archive(self, archive):
        if archive is None:
            raise PackageManagerError("install: no package name given.")
        archive = Path(archive)
        info, control_data, files = self._archive_contents(archive)
        name = info["Package"]
        version = info["Version"]

        if self.is_package_broken(name):
            raise PackageManagerError(f"install: Package {name} is in a broken state. Try uninstalling first.")
        if self.is_package_installed(name) and parse_version(version) < parse_version(self.package_version(name)):
            installed_version = self.package_version(name)
            raise PackageManagerError(
                f"install: Version {version} not the same or newer than installed version "
                f"{installed_version}.\nAborting installation."
            )

        self._check_overwrite(files, ignore_package=name)
        if self.is_package_installed(name):
            self.uninstall_package(name)

        print(f"Installing {name} version {version} ... ", end="", flush=True)
        package_info_dir = self.info_dir / name
        package_info_dir.mkdir(parents=True)
        installing_file = package_info_dir / "Installing"
        installing_file.touch()

        try:
            (package_info_dir / "control.json").write_bytes(control_data)

            # Publish the complete manifest before copying. Thus a missing Files file means that an
            # interrupted installation has not written any package payload and is safe to discard.
            manifest_file = package_info_dir / "Files"
            temporary_manifest = package_info_dir / "Files.new"
            with temporary_manifest.open("w", encoding="utf-8", newline="\n") as manifest:
                manifest.write("".join(f"{relative_name}\n" for _, relative_name, _ in files))
            temporary_manifest.replace(manifest_file)

            with tarfile.open(archive, "r:*") as package_archive:
                for archive_name, relative_name, mode in files:
                    target = self._package_file(relative_name)
                    target.parent.mkdir(parents=True, exist_ok=True)
                    if target.is_dir() and not target.is_symlink():
                        target.rmdir()
                    source = package_archive.extractfile(archive_name)
                    if source is None:
                        raise PackageManagerError(f"Could not read '{archive_name}' from archive.")
                    with source, target.open("wb") as output_file:
                        shutil.copyfileobj(source, output_file)
                    if os.name != "nt":
                        target.chmod(mode & 0o777)
            installing_file.unlink()
        except (OSError, tarfile.TarError) as error:
            raise PackageManagerError(f"Could not install package '{name}': {error}") from error
        print("done.")

    # Remove package files, retaining a journal and metadata if any removal fails.
    def uninstall_package(self, name):
        if name is None:
            raise PackageManagerError("uninstall: No package name given.")
        if not self.is_package_installed(name):
            raise PackageManagerError(f"uninstall: Package {name} is not installed.")

        package_info_dir = self.info_dir / name
        manifest_file = package_info_dir / "Files"
        if (package_info_dir / "Installing").exists() and not manifest_file.is_file():
            # Installation never copies payload before publishing Files, so there are no package files
            # to remove when an interrupted installation has no complete manifest.
            shutil.rmtree(package_info_dir)
            print(f"Uninstalling {name} ... done.")
            return

        files = self.installed_files_for_package(name)
        removed_file = package_info_dir / "FilesRemoved"
        removed = set(self._read_lines(removed_file)) if removed_file.exists() else set()
        version = self.package_version(name) if (package_info_dir / "control.json").is_file() else "unknown"
        print(f"Uninstalling {name} version {version} ... ", end="", flush=True)

        with removed_file.open("a", encoding="utf-8", newline="\n") as removal_log:
            for filename in files:
                if filename in removed:
                    continue
                package_file = self._package_file(filename)
                try:
                    if package_file.exists() or package_file.is_symlink():
                        package_file.unlink()
                except OSError as error:
                    print(f"Could not unlink {package_file}: {error}", file=sys.stderr)
                    continue

                # Already-absent files are also complete removals, as intended by the old journal design.
                print(filename, file=removal_log, flush=True)
                removed.add(filename)

        if set(files).issubset(removed):
            shutil.rmtree(package_info_dir)
            print("done.")
            return
        raise PackageManagerError(f"Failed to uninstall {name}!")

    # Fetch and decode JSON metadata from the package repository.
    def _read_json_url(self, url):
        try:
            with urlopen(secure_url(url), timeout=30) as response:
                return json.load(response)
        except (HTTPError, URLError, OSError, json.JSONDecodeError) as error:
            raise PackageManagerError(f"Could not fetch '{url}': {error}") from error

    # Return the package metadata published by the configured repository.
    def remote_packages_info(self):
        packages = self._read_json_url(self.package_index_url)
        if not isinstance(packages, list):
            raise PackageManagerError("Remote package list is not a JSON array.")
        if not all(isinstance(package, dict) for package in packages):
            raise PackageManagerError("Remote package list contains a non-object entry.")
        return packages

    # Find one named package in the remote package list.
    def remote_package_info(self, name):
        for package in self.remote_packages_info():
            if package.get("Package") == name:
                return package
        return None

    # Build the archive URL advertised for a remote package.
    def remote_package_url(self, package):
        try:
            filename = f"{package['Package']}_{package['Version']}.tar.gz"
            return secure_url(urljoin(package["Source"].rstrip("/") + "/", filename))
        except (AttributeError, KeyError, TypeError) as error:
            raise PackageManagerError("Remote package metadata is missing Package, Version, or Source.") from error

    # Download a remote package, verify every advertised checksum, and install it.
    def install_package(self, name):
        if name is None:
            raise PackageManagerError("install: no package name given.")
        print("Fetching package list ... ", end="", flush=True)
        package = self.remote_package_info(name)
        print("done.")
        if package is None:
            raise PackageManagerError(f"Package '{name}' not found!")

        version = package["Version"]
        if self.is_package_installed(name) and parse_version(version) <= parse_version(self.package_version(name)):
            installed_version = self.package_version(name)
            raise PackageManagerError(
                f"install: Remote Version {version} not newer than installed version "
                f"{installed_version}.\nAborting installation."
            )

        url = self.remote_package_url(package)
        print(f"Downloading package for {name} version {version} ... ", end="", flush=True)
        temporary_name = None
        try:
            with (
                urlopen(url, timeout=60) as response,
                tempfile.NamedTemporaryFile(delete=False, suffix=".tar.gz") as temporary_file,
            ):
                shutil.copyfileobj(response, temporary_file)
                temporary_name = Path(temporary_file.name)
            print("done.")

            for field, algorithm in (("SHA1", "sha1"), ("SHA256", "sha256"), ("MD5sum", "md5")):
                expected = package.get(field)
                if not isinstance(expected, str) or not expected:
                    raise PackageManagerError(f"Package metadata has no {field} checksum!")
                if file_digest(temporary_name, algorithm).lower() != expected.lower():
                    raise PackageManagerError(f"Package has wrong {field}!")
            self.install_archive(temporary_name)
        except (HTTPError, URLError, OSError) as error:
            raise PackageManagerError(f"Could not download '{url}': {error}") from error
        finally:
            if temporary_name is not None:
                temporary_name.unlink(missing_ok=True)


# Print the stable command summary used by both explicit and implicit help.
def show_help():
    print("Usage: bali-phy-pkg <command> [arg]")
    print(f"Commands are: {COMMANDS}")


# Dispatch the historical command interface without changing package file formats.
def run_command(manager, command, arguments):
    argument = arguments[0] if arguments else None
    if command == "install-archive":
        manager.install_archive(argument)
    elif command == "install":
        manager.install_package(argument)
    elif command == "uninstall":
        manager.uninstall_package(argument)
    elif command == "info":
        if argument is None:
            raise PackageManagerError("info: No package name given.")
        for field, value in manager.installed_package_info(argument).items():
            print(f"{field}: {value}")
    elif command == "files":
        if argument is None:
            raise PackageManagerError("files: No package name given.")
        if not manager.is_package_installed(argument):
            raise PackageManagerError(f"files: No package named \"{argument}\" is currently installed.")
        for filename in manager.installed_files_for_package(argument):
            print(filename)
    elif command == "packages":
        packages = manager.installed_packages()
        for package in packages:
            version = "broken" if manager.is_package_broken(package) else manager.package_version(package)
            print(f"{package} {version}")
        if not packages:
            print("No packages installed.")
    elif command == "untracked":
        for filename in manager.untracked_files():
            print(filename)
    elif command == "missing":
        for filename in manager.missing_files():
            print(filename)
    elif command == "installed":
        for filename in manager.installed_files():
            print(filename)
    elif command == "available":
        for package in manager.remote_packages_info():
            print(f"{package['Package']}\t{package['Version']}\t{manager.remote_package_url(package)}")
    else:
        raise PackageManagerError(f"I don't understand command '{command}'.")


# Handle help before initializing user state, then report expected operational errors concisely.
def main(argv=None, environ=None):
    argv = sys.argv[1:] if argv is None else argv
    environ = os.environ if environ is None else environ
    if not argv:
        show_help()
        return 1
    if argv[0] in ("help", "--help", "-h"):
        show_help()
        return 0

    try:
        manager = PackageManager(get_user_data_dir(environ))
        run_command(manager, argv[0], argv[1:])
        return 0
    except PackageManagerError as error:
        print(f"Error: {error}", file=sys.stderr)
        if argv[0] not in COMMANDS.split(", "):
            print(file=sys.stderr)
            show_help()
        return 1


if __name__ == "__main__":
    sys.exit(main())
