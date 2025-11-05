# План после интеграции TSyncthingManager

## Что уже сделано

- Заменён `TSyncthingAPI` на `TSyncthingManager` в `uModuleMain.pas` с привязкой событий процесса (state/console/start-failed) и сетевых событий.
- Реализованы действия: `ConnectOrStart`, `DisconnectAndStop`, `StopAndExit`, `RestartApp`; связаны с меню и треем.
- Вывод консоли процесса в `frmMain.edConsole` и дублирование в лог событий.
- Матрица доступности `TAction` синхронизирована со состояниями процесса и сети.
- Зависимость от `uModuleCore` убрана из `uModuleMain`; подключение через опции (`uFormOptions`).
- Открытие Web UI (пока фиксировано `http://127.0.0.1:8384/`).

## Оставшиеся задачи (следующий этап)

- Источник хоста/порта и TLS:
- Добавить в `uFormOptions` поля для Host/Port и флаг UseTLS; читать их при `Connect`/`ConnectOrStart`.
- Обновить `actShowWeb` для динамического URL и поддержать `https` при UseTLS.
- Полная уборка legacy `uModuleCore`:
- Удалить `uModuleCore` из `uses` остальных модулей и заменить вызовы (миграция: host/port/server URL; устаревшие кнопки).
- Улучшение UX статуса и меню:
- Отражать статус процесса в трее/подсказке (иконка/префикс «(running)»).
- Добавить явные `StartSyncthing/StopSyncthing/RestartSyncthing` (если планируется отдельными пунктами меню).
- Хардненинг и логирование:
- Диалог ошибок при `OnStartFailed` с предложением открыть опции.
- Ограничение размера консольного лога/автоскролл (порог в опциях).
- Документация:
- Обновить `doc/GPT_info.md` раздел про `uSyncthingManager` и схему запуска/остановки.

## Ключевые места изменения

- `uFormOptions.pas/.lfm` — новые поля Host/Port/UseTLS; сохранение/загрузка значений.
- `uModuleMain.pas/.lfm` — чтение опций при действиях, динамический Web UI, (опционально) новые `TAction`.
- По проекту — удалить `uModuleCore` из `uses` и заменить обращения.

## Минимальные изменения кода (примеры точек входа)

- Чтение настроек перед стартом/коннектом: `LoadManagerSettingsFromOptions` (расширить для host/port/tls).
- Открытие Web UI: заменить константу на сборку URL из опций.
