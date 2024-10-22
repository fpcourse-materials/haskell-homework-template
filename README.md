# TODO

Глобальный минимум: TODOб.

## Установка и сборка Haskell

На любой платформе достаточно получить локально систему сборки `stack`, она сама поставит вам нужный компилятор, когда вы первый раз вызовете `stack build` на репозитории с дз.
Следуйте рекомендациям с официального сайта: https://www.haskell.org/downloads/.

Также на курсе обязательно использование линтера, который будет давать подсказки по улучшению кода.
[hlint](https://github.com/ndmitchell/hlint) либо можно поставить с помощью cabal, либо через пакетный менеджер вашего дистрибутива.
Если hlint выдаёт ошибки на уже данный код, попробуйте обновить hlint (>= 3.4.1 работает).

## Порядок выполнения заданий на Haskell

### Инициализация

1. Переходите по ссылке на classroom с wiki и в первый раз выбираете себя в списке имён
2. Клонируйте репозиторий с помощью `git clone git@github.com:...`
3. Запускайте сборку, первый раз она может занять некоторое время: `stack build --test --no-run-tests`

### Выполнение работы

1. Выполняйте задания в `src/`
2. Загружайте код в интерпретатор: `stack ghci`
    * Постоянно проверяйте соответствие типов с помощью перезагрузки интерпретатора: `:r`
    * Тестируйте вручную решения, импортируя нужный модуль в интерпретаторе: `import BlockN`
3. Строгого стиля кодирования нет, но лучше ориентироваться на тесты в `test/`
4. Записывайте номера решенных заданий в `solved-tasks.txt`
    * Например, `1.1 1.2` (решены первые два задания первого блока)
5. Вызывайте `sh check.sh`, скрипт запустит линтер и тесты для перечисленных заданий
   * Избавьтесь от всех ошибок выдаваемых скриптом перед сдачей работы
6. Чуть быстрее тесты конкретных заданий запускать как `stack test --ta '1.1 1.2'`

### Сдача работы

Мы экономим время CI, поэтому не запускайте его слишком часто (желательно не больше 3-4х раз за домашку).

Скрипты сдачи запускают локальное тестирование ещё раз. Не забудьте занести сдаваемые заданий в `solved-tasks.txt`.

* Временный пуш в репозиторий: `sh submit-tmp.sh` без прогона CI
* К мягкому дедлайну: `sh submit-soft.sh`
* К жесткому дедлайну: `sh submit-hard.sh`

Отправка на проверку:
1. В вашем репозитории откроется автоматический pull request, ссылку на этот PR отправляйте на почту вашему преподавателю (для разных домашек разные треды на почте)
2. Преподаватель оставит комментарии в PR и сообщит об этом по почте
3. После того как запушите исправления и ответите на комментарии на гитхабе, снова пишите преподавателю в ответ на его письмо

## Оценивание решений

Языки различаются по выразительности.
Чем выразительнее язык, тем проще и лаконичнее можно на нем выражать свои мысли.
И тем проще писать очень плохой код.
Современные языки двигаются в сторону выразительности, а Haskell уже там.
Таким образом, важно стараться писать хороший читаемый код на Haskell, потому что очень плохой код написать на нём слишком просто.

Если код трудно читать, и если трудно убедиться глазами в его корректности, за решение будут снижаться баллы.
Старайтесь смотреть со стороны на свои решения, выбирать подходящие уровни абстракции и конструировать нужное количество промежуточных слоёв, которые делают решение понятным в каждом приближении.
За совсем безалаберное форматирование тоже могут снижаться баллы.

Поскольку мы на этом курсе живём в парадигме переизобретания программирования, для нас почти ничего не должно быть магией.
Когда вы используете тот или иной механизм, будьте готовы объяснить, как он работает.
Использование всегда только самых базовых вещей - не вариант, тогда будут снижаться баллы за выбор неправильного уровня абстракции.

Качество решений может влиять на баллы как в отрицательную сторону, так и в положительную.

## Помощь

Не стесняйтесь обращаться за помощью к вашим преподавателям, они тут как раз для этого.
Все контакты вы найдёте на вики.
Нам не сложно и в радость вам помогать, а наблюдать молчаливое непонимание как раз грустно.
Не обесценивайте свои трудности, ищите поддержки - и найдёте.

Также просите помощи у своих одногруппников, помогать интересно и полезно.

Если вы помогаете, не навредите!
Предлагайте объяснения, а не ответы.
Отталкивайтесь от потребностей собеседника, а не от желания показать себя.
Человеку и так может быть не очень здорово, очень просто закопать его ещё глубже, говоря избыток умных слов и демонстрируя свою эрудицию.

Помните: у всех разная подготовка и способности к построению определённых видов умозрительных конструкций, с этим особо ничего не сделаешь.
Но это никак вас не определяет.
Чем сложнее даётся, тем ценнее любой шажок.
Не будьте слишком требовательны к себе и двигайтесь в своём темпе.
Всё будет хорошо, гарантируем.

Не всем просто психологически обращаться за помощью.
Но это важно, без этого никак.
Попробуйте относиться к этому как к части работы и постарайтесь, оно окупится.
