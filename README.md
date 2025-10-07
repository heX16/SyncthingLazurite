### SyncthingLazurite

SyncthingLazurite is a lightweight desktop companion for Syncthing. It is a cross-platform compiled native app (Windows, Linux, macOS) optimized for very low memory and CPU usage. It connects to your local Syncthing and shows devices, folders, and recent activity in a simple window with a tray icon.

#### What it does
 - Shows your Syncthing devices and folders at a glance
 - Indicates connection status (online, paused, issues)
 - Displays a compact event log for recent activity
 - Lets you quickly connect/disconnect and view details

#### Key features
 - Cross-platform support (Windows, Linux, macOS)
 - Tray icon hint with list of online devices
 - Lightweight and simple UI
 - Native compiled app (Lazarus/FreePascal) with very low memory and CPU usage
 - SyncthingLazurite talks only to your local Syncthing instance using your API key. No data is sent to any cloud services by this app.

#### Getting started
1. Install and run Syncthing on your computer. By default it listens on `http://127.0.0.1:8384` and has an API key in its settings.
   NOTE:
   In the future, the SyncthingLazurite program will automatically download Syncthing if it does not find them, and there will also be a build containing Syncthing in the installation kit.
2. Start SyncthingLazurite.
3. Open Options and **set Syncthing API key**.
   NOTE: the app can read the key from Syncthing’s config if you point it to the config folder.
4. Click Connect. You should see your devices and folders populate within a few seconds.

#### Feedback
Issues and suggestions are welcome. Please open an issue on the repository’s tracker.

