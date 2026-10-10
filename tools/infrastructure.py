"""Сравнить или обновить общие файлы домашки из единственного шаблона."""
import argparse
import fnmatch
import os
from pathlib import Path
import shutil


IGNORED_DIRS = {".git", "dist-newstyle", "out", ".idea", ".vscode", "__pycache__"}
IGNORED_FILES = (
    "report.json", "report.md", "comment.md", "smoke.log", "repl.log",
    "cabal.project.local*", ".ghc.environment.*", "*.hi", "*.o",
    "*.dyn_hi", "*.dyn_o", "*.prof", "*.pyc", "*~", ".DS_Store", "Thumbs.db",
)


def inventory(root: Path) -> set[str]:
    files = set()
    for folder, directories, names in os.walk(root):
        directories[:] = sorted(name for name in directories if name not in IGNORED_DIRS)
        for name in names:
            if name == ".git" or any(fnmatch.fnmatch(name, pattern) for pattern in IGNORED_FILES):
                continue
            files.add((Path(folder) / name).relative_to(root).as_posix())
    return files


def homework_files(template: Path) -> set[str]:
    allowed = set()
    for line in (template / "HOMEWORK_FILES").read_text(encoding="utf-8").splitlines():
        path = line.strip()
        if not path or path.startswith("#"):
            continue
        if Path(path).is_absolute() or ".." in Path(path).parts or any(c in path for c in "*?["):
            raise ValueError(f"HOMEWORK_FILES должен содержать точные относительные пути: {path}")
        allowed.add(path)
    return allowed


def differences(template: Path, homework: Path) -> list[str]:
    allowed = homework_files(template)
    files = (inventory(template) | inventory(homework)) - allowed
    return [path for path in sorted(files)
            if not (template / path).is_file() or not (homework / path).is_file()
            or (template / path).read_bytes() != (homework / path).read_bytes()]


def sync(template: Path, homework: Path) -> int:
    allowed = homework_files(template)
    source_files = inventory(template) - allowed
    unknown = inventory(homework) - inventory(template) - allowed
    if unknown:
        raise ValueError("Сначала перенесите служебные файлы в шаблон: " + ", ".join(sorted(unknown)))
    changed = 0
    for path in sorted(source_files):
        source, target = template / path, homework / path
        if target.is_file() and source.read_bytes() == target.read_bytes():
            continue
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, target)
        changed += 1
    return changed


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("check", "sync"))
    parser.add_argument("homework", type=Path)
    args = parser.parse_args()
    template = Path(__file__).resolve().parent.parent
    homework = args.homework.resolve()
    if not homework.is_dir() or homework == template:
        parser.error("Укажите существующую папку домашки, отличную от шаблона")
    try:
        if args.action == "sync":
            print(f"{homework.name}: обновлено общих файлов — {sync(template, homework)}")
        changed = differences(template, homework)
    except (ValueError, OSError) as error:
        raise SystemExit(str(error)) from error
    if changed:
        raise SystemExit("Инфраструктура отличается от шаблона:\n" + "\n".join(changed))
    print(f"{homework.name}: инфраструктура совпадает с шаблоном — OK")


if __name__ == "__main__":
    main()
