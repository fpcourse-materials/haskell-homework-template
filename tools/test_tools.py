"""Регрессионные проверки общей инфраструктуры, без сборки и сети."""
from pathlib import Path
from tempfile import TemporaryDirectory
import subprocess
import unittest

from check_report import check_report
from infrastructure import differences, homework_files, sync


class ReportTests(unittest.TestCase):
    manifest = "-- comment\ntask 1.7 (условие)\ntask 2.12\nrule any 1 of: 1.7\n"

    def report(self, **changes):
        report = {"level1_closed": False, "tasks": [
            {"id": "1.7", "status": "TODO"},
            {"id": "2.12", "status": "PARTIAL"},
        ]}
        report.update(changes)
        return report

    def test_task_numbers_come_from_manifest(self):
        self.assertEqual(check_report(self.manifest, self.report()), 2)

    def test_missing_task_is_rejected(self):
        with self.assertRaises(ValueError):
            check_report(self.manifest, self.report(tasks=[{"id": "1.7", "status": "TODO"}]))

    def test_extra_task_is_rejected(self):
        report = self.report()
        report["tasks"].append({"id": "3.1", "status": "TODO"})
        with self.assertRaises(ValueError):
            check_report(self.manifest, report)

    def test_duplicate_task_is_rejected(self):
        report = self.report()
        report["tasks"].append(report["tasks"][0])
        with self.assertRaises(ValueError):
            check_report(self.manifest, report)

    def test_done_and_failed_are_rejected(self):
        for status in ("DONE", "FAILED"):
            with self.subTest(status=status), self.assertRaises(ValueError):
                report = self.report()
                report["tasks"][0]["status"] = status
                check_report(self.manifest, report)

    def test_closed_level_is_rejected(self):
        with self.assertRaises(ValueError):
            check_report(self.manifest, self.report(level1_closed=True))

    def test_empty_and_duplicate_manifest_are_rejected(self):
        for manifest in ("-- no tasks", "task 1.7\ntask 1.7\n"):
            with self.subTest(manifest=manifest), self.assertRaises(ValueError):
                check_report(manifest, self.report())


class InfrastructureTests(unittest.TestCase):
    def setUp(self):
        temporary = TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.template = self.root / "template"
        self.homework = self.root / "homework"
        for root in (self.template, self.homework):
            root.mkdir()
            (root / "HOMEWORK_FILES").write_text("TASKS\nsrc/Level1.hs\n", encoding="utf-8")
            (root / "Makefile").write_text("common", encoding="utf-8")
        self.write(self.homework, "src/Level1.hs", "student TODO")
        self.write(self.template, "src/Level1.hs", "template exercise")

    def write(self, root, path, content):
        target = root / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(content, encoding="utf-8")

    def test_task_files_may_differ_or_be_missing(self):
        self.write(self.template, "TASKS", "task 1.1")
        self.assertEqual(differences(self.template, self.homework), [])

    def test_shared_file_drift_is_detected(self):
        self.write(self.homework, "Makefile", "custom infrastructure")
        self.assertEqual(differences(self.template, self.homework), ["Makefile"])

    def test_missing_shared_file_is_detected(self):
        self.write(self.template, "tools/check.py", "common tool")
        self.assertEqual(differences(self.template, self.homework), ["tools/check.py"])

    def test_sync_preserves_tasks_and_git_and_build_artifacts(self):
        self.write(self.homework, "Makefile", "custom infrastructure")
        self.write(self.template, "tools/check.py", "common tool")
        self.write(self.homework, ".git/config", "remote origin")
        self.write(self.homework, "dist-newstyle/cache", "build cache")
        self.assertEqual(sync(self.template, self.homework), 2)
        self.assertEqual(differences(self.template, self.homework), [])
        self.assertEqual((self.homework / "src/Level1.hs").read_text(), "student TODO")
        self.assertEqual((self.homework / ".git/config").read_text(), "remote origin")
        self.assertEqual(sync(self.template, self.homework), 0)

    def test_unknown_files_are_rejected_before_sync(self):
        self.write(self.homework, "Makefile", "keep until checked")
        self.write(self.homework, "tools/custom.py", "unknown tool")
        with self.assertRaises(ValueError):
            sync(self.template, self.homework)
        self.assertEqual((self.homework / "Makefile").read_text(), "keep until checked")

    def test_homework_cannot_change_allowed_file_list(self):
        self.write(self.homework, "HOMEWORK_FILES", "TASKS\nsrc/Level1.hs\nMakefile\n")
        self.write(self.homework, "Makefile", "custom infrastructure")
        self.assertEqual(differences(self.template, self.homework), ["HOMEWORK_FILES", "Makefile"])

    def test_allowlist_cannot_use_wildcards_or_parent_paths(self):
        for entry in ("src/*", "../outside", "/absolute"):
            with self.subTest(entry=entry), self.assertRaises(ValueError):
                self.write(self.template, "HOMEWORK_FILES", entry)
                homework_files(self.template)


class SolutionsSyncTests(unittest.TestCase):
    def run_command(self, root, *args, check=True):
        return subprocess.run(args, cwd=root, check=check, text=True, capture_output=True)

    def test_updates_shared_files_and_preserves_solutions(self):
        with TemporaryDirectory() as temporary:
            root = Path(temporary)
            self.run_command(root, "git", "init", "-b", "main")
            self.run_command(root, "git", "config", "user.name", "Infrastructure test")
            self.run_command(root, "git", "config", "user.email", "test@example.invalid")
            (root / "Makefile").write_bytes((Path(__file__).resolve().parent.parent / "Makefile").read_bytes())
            (root / "src").mkdir()
            solutions = ("Level1.hs", "Level2.hs", "Level3.hs", "hw.lam")
            for name in solutions:
                (root / "src" / name).write_text("student TODO")
            self.run_command(root, "git", "add", ".")
            self.run_command(root, "git", "commit", "-m", "template")
            self.run_command(root, "git", "checkout", "-b", "solutions")
            for name in solutions:
                (root / "src" / name).write_text("reference solution")
            (root / "obsolete-tool.py").write_text("old infrastructure")
            self.run_command(root, "git", "add", ".")
            self.run_command(root, "git", "commit", "-m", "solutions")
            self.run_command(root, "git", "checkout", "main")
            (root / "shared-tool.py").write_text("new infrastructure")
            (root / "TASKS").write_text("task 1.1")
            self.run_command(root, "git", "add", ".")
            self.run_command(root, "git", "commit", "-m", "update template")
            self.run_command(root, "git", "checkout", "solutions")
            self.run_command(root, "make", "solutions-sync")
            self.assertEqual((root / "shared-tool.py").read_text(), "new infrastructure")
            self.assertEqual((root / "TASKS").read_text(), "task 1.1")
            self.assertFalse((root / "obsolete-tool.py").exists())
            for name in solutions:
                self.assertEqual((root / "src" / name).read_text(), "reference solution")
                main = self.run_command(root, "git", "show", f"main:src/{name}").stdout
                self.assertEqual(main, "student TODO")
            self.run_command(root, "git", "checkout", "-f", "main")
            refused = self.run_command(root, "make", "solutions-sync", check=False)
            self.assertNotEqual(refused.returncode, 0)


if __name__ == "__main__":
    unittest.main()
