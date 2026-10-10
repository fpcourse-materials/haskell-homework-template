"""Проверить стартовый отчёт домашки по её манифесту TASKS."""
import json
from pathlib import Path


def check_report(manifest: str, report: dict) -> int:
    expected = []
    for line in manifest.splitlines():
        words = line.strip().split()
        if words and words[0] == "task":
            if len(words) < 2:
                raise ValueError("В TASKS пропущен идентификатор задачи")
            expected.append(words[1])
    if not expected or len(set(expected)) != len(expected):
        raise ValueError("TASKS должен содержать непустой список задач без повторов")
    rows = report["tasks"]
    actual = [row["id"] for row in rows]
    if set(actual) != set(expected) or len(actual) != len(expected):
        raise ValueError(f"Задачи отчёта не совпадают с TASKS: {actual}")
    if report["level1_closed"] is not False:
        raise ValueError("В заготовке обязательный уровень уже закрыт")
    # Готовые ветки фейдинга могут закрыть отдельные проверки, но не всю задачу.
    if any(row["status"] not in {"TODO", "PARTIAL"} for row in rows):
        raise ValueError("В заготовке допустимы только статусы TODO и PARTIAL")
    return len(expected)


if __name__ == "__main__":
    try:
        count = check_report(
            Path("TASKS").read_text(encoding="utf-8"),
            json.loads(Path("report.json").read_text(encoding="utf-8")),
        )
    except (ValueError, KeyError, OSError) as error:
        raise SystemExit(f"Проверка заготовки: {error}") from error
    print(f"Студенческий шаблон: {count} задач, обязательные не закрыты — OK")
