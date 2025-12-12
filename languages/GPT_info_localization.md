## Памятка для GPT по обновлению локализаций (SyncthingLazurite)

Цель этого файла — быстро ввести GPT‑помощника в контекст локализаций проекта и дать чёткие шаги для обновления переводов.

## Базовая информация о проекте

Текущий проект: SyncthingLazurite — легковесный нативный GUI (компаньон) для Syncthing.
NOTE: Syncthing - программа для синхронизации файлов.

Написан на Lazarus/FreePascal.

### Файлы и роли локализации
- `.pot` (шаблон): набор `msgid` без перевода. Единственный источник истины — `languages/SyncthingLazurite.pot`, где `msgid` — специальные идентификаторы UI.
- `.po` (переводы): для каждого `msgid` хранится `msgstr`. В проекте: EN — `languages/SyncthingLazurite.en.pot`, RU — `languages/SyncthingLazurite.ru.po`.
- Структура записи: (необязательные) `#:` и `msgctxt`, затем `msgid` и `msgstr`; единый заголовок — запись с пустым `msgid ""` (UTF‑8).

### Кратко о системе переводов в Lazarus
- Gettext‑подход: строки извлекаются из `.lfm` и `resourcestring` в `.pot`; в `#:` часто путь вида `tform.control.caption`.
- Загрузка переводов: подключите `DefaultTranslator` в главном `*.lpr` — он подхватит `ProjectName.<lang>.po` из `languages/` по локали системы.
- Что переводится: UI‑свойства (Caption, Hint и т. п.) и `resourcestring` (через `TranslateUnitResourceStrings`, вызывается автоматически).
- Формат: Lazarus читает `.po` напрямую (без `.mo`); имена файлов соответствуют имени проекта и коду языка.
- Обновления: при изменениях форм/строк обновляйте `.pot` и синхронизируйте `.po`.



### Правила сравнения и обновления
- Уникальность ключа: использовать только `msgid` (игнорировать `msgctxt`) при сверке наборов ключей.
- Обработка устаревших: ключи, которых нет в источнике, НУЖНО удалять из языковых файлов (не помечать `#~`, не оставлять).
- `fuzzy`: не использовать — новые и обновлённые записи добавлять как подтверждённые (без `#, fuzzy`).
- Заголовок (`msgid ""`) не учитывать как ключ, но сохранять корректным (Content-Type UTF‑8).
- Числа, URL и технические маркеры (например, `8384`, `https://syncthing.net/`, `...`, `SyncthingLazurite`) в переводах оставлять как есть.
- `msgctxt` (если присутствует) сохранять в файлах, но он не влияет на сравнение наборов ключей.

### Скрипт для сверки ключей

В репозитории есть утилита: `scripts/compare_locales.py`

Назначение: сравнить два файла `.po/.pot` и вывести:
- Missing — есть в источнике, нет в целевом,
- Obsolete — есть в целевом, нет в источнике.

Запуск (Windows/повсюду):
```bash
python scripts/compare_locales.py --source languages/SyncthingLazurite.pot --target languages/SyncthingLazurite.en.pot
python scripts/compare_locales.py --source languages/SyncthingLazurite.pot --target languages/SyncthingLazurite.ru.po
```
Особенности:
- Сверка идёт по `msgid` (контекст игнорируется).
- Скрипт не проверяет заполненность `msgstr`, только наличие ключа. То есть ключ может «не считаться Missing», но его перевод в `msgstr` всё ещё может быть пустым — это нужно проверить вручную.

### Процедура обновления переводов (пошагово)
1) Сравнить EN и RU с источником:
```bash
python scripts/compare_locales.py --source languages/SyncthingLazurite.pot --target languages/SyncthingLazurite.en.pot
python scripts/compare_locales.py --source languages/SyncthingLazurite.pot --target languages/SyncthingLazurite.ru.po
```
2) Для каждого целевого файла:
- Удалить ключи из списка Obsolete.
- Добавить отсутствующие ключи из списка Missing, сохранив структуру блока:
  - (опциональный) `#:` комментарий‑источник
  - (опциональный) `msgctxt "..."` — если есть в источнике, переносим
  - `msgid "..."` (идентификатор)
  - `msgstr "..."` (перевод)
- Проверить, что заголовок (`msgid ""`) сохранён и корректен (UTF‑8).
3) Заполнить `msgstr`:
- Английский (`.en.pot`): давать человекочитаемые строки по терминологии UI (например: `grpDevices` → `Devices`, `actDisconnectAndStop` → `Disconnect and stop`, `btnClose` → `Close`).
- Русский (`.ru.po`): стиль консистентен с существующим:
  - действия — императив: «Показать», «Остановить», «Отключиться»/«Отключить» по смыслу,
  - сущности согласованы: `Devices` → «Устройства», `Events` → «События», `Folders` → «Папки»,
  - уже переведённые строки — не менять без необходимости.
4) Повторно запустить сравнение, убедиться, что:
```plain
Missing (0)
Obsolete (0)
```
для EN и RU.

### Примеры соответствий (ориентиры)
- `btnClose` → EN: `Close`, RU: `Закрыть`
- `grpDevices` → EN: `Devices`, RU: `Устройства`
- `grpEvents` → EN: `Events`, RU: `События`
- `grpFolders` → EN: `Folders`, RU: `Папки`
- `edPortNumber` → EN: `Port number`, RU: «Номер порта»
- `cLanguageName` / `cLanguageNameEng`:
  - `cLanguageName` — название языка на родном языке (включая собственное письмо), пример: RU → «Русский», zh-CN → «简体中文»
  - `cLanguageNameEng` — название языка на английском, пример: RU → `Russian`, zh-CN → `Chinese (Simplified)`

### Возможные доработки (при необходимости)
- Расширить `scripts/compare_locales.py` режимом «auto‑fix»:
  - опция для удаления Obsolete из целевого файла,
- Добавить проверку пустых `msgstr` (отдельный отчёт `Untranslated`).

Этого достаточно, чтобы с «чистого листа» обновить локализации: сначала сверка через скрипт, затем — добавление/удаление по отчёту, заполнение переводов и финальная проверка до состояния `Missing (0) / Obsolete (0)`. 


