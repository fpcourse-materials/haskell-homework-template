# Единая точка входа домашки. `make` без цели печатает список целей.
#
# make build                  собрать проект
# make check                  линтер и тесты задач из solved-tasks.txt
# make check ONLY="1.1 1.2"   то же, но только для перечисленных задач
# make submit                 локальная проверка, коммит «submit: …» и пуш — версия к ревью
# make tmp                    коммит «[no ci] tmp» и пуш без прогона CI
# make release MSG="…"        для преподавателей: обновить решения и опубликовать шаблон

ONLY ?=
SOLVED = $(shell cat solved-tasks.txt)

.PHONY: help build lint test check submit tmp release

help:
	@sed -n '3,8p' Makefile | sed 's/^# //'

# Собирает только то, что нужно для проверки: библиотеку и тесты домашки.
build:
	cabal build homework-test

# Подсказки линтера не влияют на статус задач, но их видит ревью; `make check` показывает их каждый раз.
lint:
	hlint src test

test:
	cabal test homework-test --test-options="$(ONLY) $(SOLVED)"

check:
	-$(MAKE) --no-print-directory lint
	$(MAKE) --no-print-directory test

submit: check
	git add -A
	git commit -m "submit: $(ONLY) $(SOLVED)" --allow-empty
	git push origin main
	@echo "Отчёт CI появится в вашем pull request."

tmp:
	git add -A
	git commit -m "[no ci] tmp" --allow-empty
	git push origin main
	@echo "Не забудьте сдать версию через make submit."

# Для преподавателей. Ожидает remote origin (репозиторий домашки), remote template
# (внутренний шаблон) и remote public (шаблон в организации студентов); ветка solutions — эталон.
release:
	git push origin main:main
	git config pull.rebase false
	git checkout solutions
	hlint src test
	$(MAKE) --no-print-directory test
	git commit -am "[no ci] $(or $(MSG),Update)" --allow-empty
	git pull template main --no-edit
	git push origin solutions:solutions
	git checkout main
	git merge solutions -m "[no ci] Merged with solutions"
	hlint src test
	cabal build all --ghc-options=-Werror
	git push origin main:main
	git push public main:main
	git checkout solutions
