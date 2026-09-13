#!/usr/bin/env sh
# Сдать версию: локальная проверка, коммит с префиксом "submit:" и пуш.
# Префикс говорит авторевью, что эту версию стоит посмотреть (квота на домашку ограничена).
set -e
sh check.sh "$1"
if git log -n 1 --format=%s | grep -q '^submit:';
then
  git commit -a -m "submit: $1 $(cat solved-tasks.txt)" --amend
else
  git commit -a -m "submit: $1 $(cat solved-tasks.txt)" --allow-empty
fi
git push --force origin main:main
echo "Good job! The CI report will appear in your pull request."
