# Единая точка входа домашки. `make` без цели печатает список целей.
#
# make build                  собрать проект
# make repl                   интерпретатор: GHCi со всеми уровнями (Haskell) или λ-REPL с src/hw.lam
# make repl FILE=путь.lam     λ-REPL с другим файлом, например FILE=lambda-engine/tutorial.lam
# make check                  линтер и тесты всех задач; код выхода — закрыт ли уровень 1
# make check ONLY="1.1 1.2"   то же, но только для перечисленных задач
# make check FILE=путь.lam    проверить другой .lam-файл (без манифеста TASKS), например учебный
# make submit [MSG="…"]       сборка, проверка для сведения, коммит и пуш

ONLY ?=
MSG ?=
FILE ?=
.DEFAULT_GOAL := help

.PHONY: help build repl lint test check submit

help:
	@sed -n '3,9p' Makefile | sed 's/^# //'

# Собирает только то, что нужно для проверки: библиотеку и тесты домашки.
build:
	cabal build homework-test

# λ-домашку узнаём по src/hw.lam (или по явному FILE). GHCi открывает модуль Repl,
# который импортирует все уровни, поэтому в области видимости сразу вся домашка.
# -v0 прячет служебные сообщения cabal; первая сборка интерпретатора идёт молча.
repl:
	@if [ -n "$(FILE)" ] || [ -f src/hw.lam ]; then \
	  echo "Готовлю интерпретатор λ-термов…"; \
	  cabal build -v0 lambda && cabal run -v0 lambda -- load $(or $(FILE),src/hw.lam); \
	else \
	  cabal repl -v0 homework; \
	fi

# Подсказки линтера не влияют на статус задач, но их видит ревью; `make check` показывает их каждый раз.
# В λ-домашке кода на Haskell у студента нет, и линтер не запускается — ставить hlint не нужно.
lint:
	@if [ -f src/hw.lam ]; then \
	  echo "λ-домашка: линтер Haskell не запускается"; \
	else \
	  hlint src test; \
	fi

# С FILE раннер проверяет указанный .lam-файл вместо src/hw.lam; манифест TASKS
# к нему не относится, поэтому отключается (HASKELL_TASKS указывает на несуществующий файл).
test:
	$(if $(FILE),LAMBDA_FILE=$(FILE) HASKELL_TASKS=/nonexistent) cabal test homework-test --test-options="$(ONLY)"

check:
	-$(MAKE) --no-print-directory lint
	$(MAKE) --no-print-directory test

# Любой пуш в main — текущая версия домашки: её проверяет CI и её же смотрит ревью.
# Проверка запускается для сведения, блокирует только несобирающийся код.
# Перед пушем подтягиваются правки, которые преподаватели вносят в репозиторий
# (исправления тестов, прелюдии, проверялки): ваши решения они не трогают.
submit: build
	-$(MAKE) --no-print-directory check
	git add -A
	git commit -m "$(or $(MSG),Сдача)" --allow-empty
	git pull --rebase --quiet origin main
	git push origin main
	@echo "Отчёт CI появится в вашем pull request."

# Для преподавателей. origin с эталоном приватный, PUBLIC_REPO — публичный шаблон студентов.
# SOLUTIONS_SOURCE — необязательная папка эталона; обновляются только файлы решений.
# make checker-test          тесты λ-проверялки, включая регрессионные примеры
# make checker-check         тесты, строгая сборка и линтер λ-проверялки
# make tools-test            проверки общих служебных инструментов
# make check-report          проверить статусы заглушек в report.json по TASKS
# make verify-build          строгая сборка всех компонентов без публикации
# make infrastructure-check HOMEWORK=путь  проверить совпадение общих файлов с шаблоном
# make infrastructure-sync HOMEWORK=путь   обновить общие файлы домашки из шаблона
# make release-status        состояние GitHub и обновления внутреннего шаблона
# make public-template PUBLIC_REPO=организация/имя  создать и проверить публичный шаблон
# make solutions-sync        обновить общие файлы и условия из main в ветке solutions
# make publish-template MSG="…"  проверить, сохранить и запушить общий шаблон
# make release MSG="…"        обновить решения и опубликовать шаблон
GH ?= gh
PUBLIC_REPO ?=
SOLUTIONS_SOURCE ?=
HOMEWORK ?=

.PHONY: checker-test checker-check tools-test check-report verify-build infrastructure-check infrastructure-sync release-status public-template solutions-sync publish-template release

checker-test:
	cabal test lambda-test --test-show-details=direct

checker-check: checker-test
	cabal build lambda-engine --enable-tests --ghc-options=-Werror
	hlint lambda-engine/src lambda-engine/app lambda-engine/test

tools-test:
	python3 tools/test_tools.py

check-report:
	python3 tools/check_report.py

verify-build:
	cabal build all --ghc-options=-Werror

infrastructure-check:
	@test -n "$(HOMEWORK)" || { echo 'Укажите HOMEWORK=путь'; exit 1; }
	python3 tools/infrastructure.py check "$(HOMEWORK)"

infrastructure-sync:
	@test -n "$(HOMEWORK)" || { echo 'Укажите HOMEWORK=путь'; exit 1; }
	python3 tools/infrastructure.py sync "$(HOMEWORK)"

publish-template: tools-test verify-build
	@test -n "$(MSG)" || { echo 'Укажите MSG=описание'; exit 1; }
	@test "$$(git branch --show-current)" = main
	@test "$$(git remote get-url origin)" = 'git@github.com:fpcourse-materials/haskell-homework-template.git'
	@test "$$($(GH) repo view --json nameWithOwner --jq '.nameWithOwner')" = 'fpcourse-materials/haskell-homework-template'
	git diff --check
	git fetch origin main
	git merge-base --is-ancestor origin/main HEAD
	git add -A
	@if ! git diff --cached --quiet; then git commit -m "$(MSG)"; fi
	git push origin main:main

release-status:
	git fetch origin main solutions
	git fetch template main
	$(GH) repo view --json nameWithOwner,isPrivate,url
	git log --oneline main..template/main
	git diff --stat main...template/main

public-template:
	@test -n "$(PUBLIC_REPO)" || { echo 'Укажите PUBLIC_REPO=организация/имя'; exit 1; }
	@test "$$($(GH) repo view --json isPrivate --jq '.isPrivate')" = true
	@if ! $(GH) repo view "$(PUBLIC_REPO)" --json nameWithOwner >/dev/null 2>&1; then \
	  $(GH) repo create "$(PUBLIC_REPO)" --public; \
	fi
	@test "$$($(GH) repo view "$(PUBLIC_REPO)" --json isPrivate --jq '.isPrivate')" = false
	$(GH) repo edit "$(PUBLIC_REPO)" --template
	@if ! git remote get-url public >/dev/null 2>&1; then git remote add public "git@github.com:$(PUBLIC_REPO).git"; fi
	@test "$$(git remote get-url public)" = "git@github.com:$(PUBLIC_REPO).git"
	$(GH) repo view "$(PUBLIC_REPO)" --json nameWithOwner,isPrivate,isTemplate,url

solutions-sync:
	@test "$$(git branch --show-current)" = solutions
	git restore --source=main --staged --worktree -- . ':(exclude)src/Level1.hs' ':(exclude)src/Level2.hs' ':(exclude)src/Level3.hs' ':(exclude)src/hw.lam'
	@if [ -n "$(SOLUTIONS_SOURCE)" ]; then \
	  if [ -f "$(SOLUTIONS_SOURCE)/src/hw.lam" ]; then cp "$(SOLUTIONS_SOURCE)/src/hw.lam" src/; \
	  else cp "$(SOLUTIONS_SOURCE)"/src/Level*.hs src/; fi; \
	fi

release:
	@test "$$(git branch --show-current)" = main
	@test -n "$(PUBLIC_REPO)" || { echo 'Укажите PUBLIC_REPO=организация/имя'; exit 1; }
	@test "$$($(GH) repo view --json isPrivate --jq '.isPrivate')" = true
	@if [ -n "$(SOLUTIONS_SOURCE)" ]; then test -d "$(SOLUTIONS_SOURCE)/src"; fi
	git config pull.rebase false
	git pull template main --no-edit
	$(MAKE) --no-print-directory lint
	$(MAKE) --no-print-directory verify-build
	@rm -f report.json
	HASKELL_TEST_REPORT_JSON=report.json $(MAKE) --no-print-directory test || $(MAKE) --no-print-directory check-report
	git diff --check
	@if ! git diff --quiet; then git commit -am "[no ci] $(or $(MSG),Update homework template)"; fi
	git checkout solutions
	git restore --source=main --staged --worktree -- Makefile
	$(MAKE) --no-print-directory solutions-sync
	$(MAKE) --no-print-directory lint
	$(MAKE) --no-print-directory verify-build
	HASKELL_TEST_STRICT=1 $(MAKE) --no-print-directory test
	git diff --check
	@if ! git diff --quiet HEAD; then git commit -am "[no ci] $(or $(MSG),Update homework solutions)"; fi
	git push origin solutions:solutions
	git checkout main
	$(MAKE) --no-print-directory public-template PUBLIC_REPO="$(PUBLIC_REPO)"
	git push origin main:main
	git push public main:main
