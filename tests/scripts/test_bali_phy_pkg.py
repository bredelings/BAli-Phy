#!/usr/bin/env python3

import contextlib
import hashlib
import importlib.util
import io
import json
from pathlib import Path
import sys
import tarfile
import tempfile
import unittest
from unittest import mock


SCRIPT = Path(__file__).resolve().parents[2] / "scripts" / "bali-phy-pkg.py"


# Load the installed-script source as a module so tests can isolate its user data root.
def load_package_manager():
    spec = importlib.util.spec_from_file_location("bali_phy_pkg", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


MODULE = load_package_manager()


# Add one in-memory regular file to a tar archive with deterministic metadata.
def add_tar_file(archive, name, contents):
    data = contents if isinstance(contents, bytes) else contents.encode("utf-8")
    member = tarfile.TarInfo(name)
    member.size = len(data)
    member.mode = 0o644
    archive.addfile(member, io.BytesIO(data))


# Construct the small package archives needed by the package-manager behavior tests.
def make_package_archive(directory, name, version, files, archive_name=None):
    archive_name = archive_name or f"{name}_{version}.tar.gz"
    filename = Path(directory) / archive_name
    control = {"Package": name, "Version": version, "Source": Path(directory).as_uri()}
    with tarfile.open(filename, "w:gz") as archive:
        add_tar_file(archive, "control.json", json.dumps(control))
        for relative_name, contents in files.items():
            add_tar_file(archive, f"files/{relative_name}", contents)
    return filename


# Construct a standard package-version-rooted Hackage source archive.
def make_hackage_archive(directory, name, version, files):
    package_id = f"{name}-{version}"
    filename = Path(directory) / f"{package_id}.tar.gz"
    with tarfile.open(filename, "w:gz") as archive:
        for relative_name, contents in files.items():
            add_tar_file(archive, f"{package_id}/{relative_name}", contents)
    return filename


# Return in-memory URL responses while retaining the Request headers and exact URLs under test.
def urlopen_from(responses):
    # Resolve the URL exactly as urllib would while returning an isolated byte stream per request.
    def open_response(request, timeout):
        url = request.full_url if hasattr(request, "full_url") else request
        if url not in responses:
            raise AssertionError(f"unexpected URL: {url}")
        return io.BytesIO(responses[url])

    return open_response


# These tests protect archive compatibility and package-state recovery; they can be removed only
# if bali-phy-pkg adopts a different on-disk package format with equivalent end-to-end coverage.
class PackageManagerTests(unittest.TestCase):
    # Suppress routine command progress while retaining it for inspection after a failed assertion.
    def setUp(self):
        self.command_output = io.StringIO()
        self.command_errors = io.StringIO()
        self.stdout_redirect = contextlib.redirect_stdout(self.command_output)
        self.stderr_redirect = contextlib.redirect_stderr(self.command_errors)
        self.stdout_redirect.__enter__()
        self.stderr_redirect.__enter__()

    # Restore the test runner's streams after each isolated package-manager operation.
    def tearDown(self):
        self.stderr_redirect.__exit__(None, None, None)
        self.stdout_redirect.__exit__(None, None, None)

    # Preserve local install, replacement, version ordering, and complete uninstall as one lifecycle.
    def test_local_archive_lifecycle(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            manager = MODULE.PackageManager(directory / "user-data")
            first = make_package_archive(
                directory,
                "Example",
                "1.2.0",
                {"haskell/Example.hs": "first\n", "node/leaf": "leaf\n"},
            )
            manager.install_archive(first)

            installed_file = manager.packages_dir / "haskell" / "Example.hs"
            self.assertEqual(installed_file.read_text(encoding="utf-8"), "first\n")
            self.assertEqual(manager.installed_packages(), ["Example"])
            self.assertEqual(
                manager.installed_files_for_package("Example"),
                ["haskell/Example.hs", "node/leaf"],
            )
            self.assertEqual(manager.missing_files(), [])
            self.assertEqual(manager.untracked_files(), [])

            replacement = make_package_archive(
                directory,
                "Example",
                "1.2",
                {"haskell/Example.hs": "replacement\n", "node": "regular file\n"},
                archive_name="replacement.tar.gz",
            )
            manager.install_archive(replacement)
            self.assertEqual(installed_file.read_text(encoding="utf-8"), "replacement\n")
            self.assertEqual((manager.packages_dir / "node").read_text(encoding="utf-8"), "regular file\n")

            older = make_package_archive(
                directory,
                "Example",
                "1.1.9",
                {"haskell/Example.hs": "older\n"},
                archive_name="older.tar.gz",
            )
            with self.assertRaisesRegex(MODULE.PackageManagerError, "not the same or newer"):
                manager.install_archive(older)
            self.assertEqual(installed_file.read_text(encoding="utf-8"), "replacement\n")

            manager.uninstall_package("Example")
            self.assertFalse(manager.is_package_installed("Example"))
            self.assertFalse(installed_file.exists())

    # A failed copy must retain its intended manifest and broken marker so that inspection and
    # cleanup remain possible; broader command tests cannot exercise interrupted filesystem writes.
    def test_failed_install_is_broken_and_recoverable(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            manager = MODULE.PackageManager(directory / "user-data")
            blocked = manager.packages_dir / "blocked"
            blocked.mkdir()
            sentinel = blocked / "sentinel.txt"
            sentinel.write_text("user data\n", encoding="utf-8")
            package = make_package_archive(directory, "Example", "1.0", {"blocked": "package data\n"})

            with self.assertRaisesRegex(MODULE.PackageManagerError, "Could not install package"):
                manager.install_archive(package)
            self.assertTrue(manager.is_package_broken("Example"))
            self.assertEqual(manager.installed_files_for_package("Example"), ["blocked"])
            self.assertEqual(manager.missing_files(), ["blocked"])
            self.assertEqual(sentinel.read_text(encoding="utf-8"), "user data\n")

            sentinel.unlink()
            blocked.rmdir()
            manager.uninstall_package("Example")
            self.assertFalse(manager.is_package_installed("Example"))

    # Ensure conflict checks reject writes before uninstalling or modifying an existing package.
    def test_conflicting_and_untracked_files_are_not_overwritten(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            manager = MODULE.PackageManager(directory / "user-data")
            first = make_package_archive(directory, "First", "1.0", {"shared.txt": "owned\n"})
            manager.install_archive(first)

            conflicting = make_package_archive(
                directory,
                "Second",
                "1.0",
                {"shared.txt": "conflict\n"},
                archive_name="conflicting.tar.gz",
            )
            with self.assertRaisesRegex(MODULE.PackageManagerError, "conflicting file"):
                manager.install_archive(conflicting)
            self.assertEqual((manager.packages_dir / "shared.txt").read_text(encoding="utf-8"), "owned\n")
            self.assertFalse(manager.is_package_installed("Second"))

            untracked_file = manager.packages_dir / "untracked.txt"
            untracked_file.write_text("user data\n", encoding="utf-8")
            untracked = make_package_archive(
                directory,
                "Third",
                "1.0",
                {"untracked.txt": "package data\n"},
                archive_name="untracked.tar.gz",
            )
            with self.assertRaisesRegex(MODULE.PackageManagerError, "untracked file"):
                manager.install_archive(untracked)
            self.assertEqual(untracked_file.read_text(encoding="utf-8"), "user data\n")

    # Retain enough uninstall state to resume after a failed removal, including already-missing files.
    def test_uninstall_journal_survives_failed_removal(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            manager = MODULE.PackageManager(directory / "user-data")
            package = make_package_archive(directory, "Example", "1.0", {"gone.txt": "x", "blocked.txt": "y"})
            manager.install_archive(package)

            (manager.packages_dir / "gone.txt").unlink()
            blocked = manager.packages_dir / "blocked.txt"
            blocked.unlink()
            blocked.mkdir()

            with self.assertRaisesRegex(MODULE.PackageManagerError, "Failed to uninstall"):
                manager.uninstall_package("Example")
            self.assertTrue(manager.is_package_broken("Example"))
            self.assertEqual(
                (manager.info_dir / "Example" / "FilesRemoved").read_text(encoding="utf-8"),
                "gone.txt\n",
            )

            blocked.rmdir()
            manager.uninstall_package("Example")
            self.assertFalse(manager.is_package_installed("Example"))

    # Protect repository-relative archives and checksum validation without a live package server;
    # this can go when an external package client provides equivalent verified repository handling.
    def test_remote_install_from_local_repository(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            package = make_package_archive(directory, "Remote", "2.0", {"Remote.hs": "module Remote where\n"})
            package_data = package.read_bytes()
            metadata = {
                "Package": "Remote",
                "Version": "2.0",
                "SHA1": hashlib.sha1(package_data).hexdigest(),
                "SHA256": hashlib.sha256(package_data).hexdigest(),
                "MD5sum": hashlib.md5(package_data).hexdigest(),
            }
            package_index = directory / "Packages"
            package_index.write_text(json.dumps([metadata]), encoding="utf-8")

            manager = MODULE.PackageManager(directory / "user-data", package_index.as_uri())
            manager.install_package("Remote")
            self.assertEqual(
                (manager.packages_dir / "Remote.hs").read_text(encoding="utf-8"),
                "module Remote where\n",
            )

            metadata["SHA256"] = "0" * 64
            package_index.write_text(json.dumps([metadata]), encoding="utf-8")
            invalid_manager = MODULE.PackageManager(directory / "invalid-user-data", package_index.as_uri())
            with self.assertRaisesRegex(MODULE.PackageManagerError, "wrong SHA256"):
                invalid_manager.install_package("Remote")
            self.assertFalse(invalid_manager.is_package_installed("Remote"))

    # Protect exact-version caching, revised Cabal metadata, source manifests, and offline reuse;
    # this can be removed if a future Hackage client provides equivalent verified cache semantics.
    def test_hackage_source_fetch_and_offline_reuse(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            name = "example-package"
            version = "1.2.3"
            package_id = f"{name}-{version}"
            original_cabal = f"name: {name}\nversion: {version}\nlicense-file: LICENSE\n"
            revised_cabal = (
                f"name: {name}\nversion: {version}\n"
                "license-files: LICENSE, NOTICE.txt -- bundled notices\n"
            ).encode("utf-8")
            archive = make_hackage_archive(
                directory,
                name,
                version,
                {
                    f"{name}.cabal": original_cabal,
                    "LICENSE": "license\n",
                    "NOTICE.txt": "notice\n",
                    "src/Example.hs": "module Example where\n",
                },
            )
            base_url = "https://hackage.test"
            revisions_url = f"{base_url}/package/{package_id}/revisions/"
            source_url = f"{base_url}/package/{package_id}/{package_id}.tar.gz"
            cabal_url = f"{base_url}/package/{package_id}/revision/1.cabal"
            responses = {
                revisions_url: json.dumps(
                    [{"number": 1, "sha256": hashlib.sha256(revised_cabal).hexdigest()}]
                ).encode("utf-8"),
                source_url: archive.read_bytes(),
                cabal_url: revised_cabal,
            }

            manager = MODULE.PackageManager(directory / "user-data", hackage_url=base_url)
            with mock.patch.object(MODULE, "urlopen", side_effect=urlopen_from(responses)):
                cache_dir = manager.fetch_hackage(name, version)

            self.assertEqual((cache_dir / "source" / f"{name}.cabal").read_bytes(), revised_cabal)
            manifest = json.loads((cache_dir / "manifest.json").read_text(encoding="utf-8"))
            self.assertEqual(manifest["cabal-revision"]["revision"], 1)
            self.assertEqual(manifest["license-files"], ["LICENSE", "NOTICE.txt"])
            self.assertIn("src/Example.hs", [file_info["path"] for file_info in manifest["source-files"]])

            with mock.patch.object(MODULE, "urlopen", side_effect=AssertionError("network used for cache hit")):
                self.assertEqual(manager.fetch_hackage(name, version), cache_dir)

            (cache_dir / "source" / "src" / "Example.hs").write_text("tampered\n", encoding="utf-8")
            with self.assertRaisesRegex(MODULE.PackageManagerError, "extracted source"):
                manager.fetch_hackage(name, version)

    # The Hackage extractor must reject traversal and links before publishing any source tree;
    # this remains necessary while extraction is implemented locally rather than by a trusted client.
    def test_hackage_archive_rejects_unsafe_entries(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            manager = MODULE.PackageManager(directory / "user-data")
            package_id = "unsafe-1.0"

            for label, member in (
                ("traversal", tarfile.TarInfo(f"{package_id}/../../escaped")),
                ("link", tarfile.TarInfo(f"{package_id}/link")),
            ):
                archive_filename = directory / f"{label}.tar.gz"
                member_data = b"bad"
                if label == "link":
                    member.type = tarfile.SYMTYPE
                    member.linkname = "../../escaped"
                else:
                    member.size = len(member_data)
                with tarfile.open(archive_filename, "w:gz") as archive:
                    archive.addfile(member, None if label == "link" else io.BytesIO(member_data))

                with self.subTest(label=label):
                    with self.assertRaisesRegex(MODULE.PackageManagerError, "invalid path|not a regular file"):
                        manager._extract_hackage_archive(
                            archive_filename,
                            directory / f"source-{label}",
                            package_id,
                        )
                    self.assertFalse((directory / "escaped").exists())

    # Prevent a malformed remote archive from writing outside the package directory.
    def test_archive_path_traversal_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            archive_name = directory / "malicious.tar.gz"
            with tarfile.open(archive_name, "w:gz") as archive:
                add_tar_file(archive, "control.json", '{"Package":"Bad","Version":"1.0"}')
                add_tar_file(archive, "files/../../escaped.txt", "escaped")

            manager = MODULE.PackageManager(directory / "user-data")
            with self.assertRaisesRegex(MODULE.PackageManagerError, "invalid path"):
                manager.install_archive(archive_name)
            self.assertFalse((directory / "escaped.txt").exists())

    # Keep user-data discovery consistent with native BAli-Phy on Unix-like and native Windows shells.
    def test_user_data_paths_and_version_normalization(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            self.assertEqual(
                MODULE.get_user_data_dir({"HOME": str(directory)}),
                directory / ".local" / "share" / "bali-phy",
            )
            self.assertEqual(
                MODULE.get_user_data_dir({"LOCALAPPDATA": str(directory)}),
                directory / "bali-phy",
            )
            self.assertEqual(MODULE.parse_version("1.2"), MODULE.parse_version("1.2.0"))
            self.assertLess(MODULE.parse_version("1.2.9"), MODULE.parse_version("1.3"))

    # Keep help usable before a user data directory exists and preserve the command's exit statuses.
    def test_help_does_not_require_user_state(self):
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            self.assertEqual(MODULE.main(["--help"], {}), 0)
            self.assertEqual(MODULE.main([], {}), 1)
        self.assertIn("Usage: bali-phy-pkg", output.getvalue())


if __name__ == "__main__":
    unittest.main()
