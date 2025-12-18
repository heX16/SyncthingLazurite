# SyncthingLazurite

[SyncthingLazurite](https://github.com/heX16/SyncthingLazurite) is a lightweight desktop companion for [Syncthing](https://syncthing.net/), an open‑source tool that securely synchronizes files across your devices.
It shows all the essentials in a simple window with a tray icon.

## What it does
 - Shows your Syncthing devices and folders
 - Indicates connection status (online, paused, issues)
 - Displays a compact event log for recent activity
 - Lets you start and stop the Syncthing service

## Key features
 - Classic interface
 - Native app
 - Tray icon
 - Cross-platform support (Windows, Linux, macOS)
 - Very low memory and CPU usage (no embedded browser, no Electron)
 - Lightweight, fast and simple UI
 - Native UI using OS widgets
 - Localization support. Currently translated into 10 languages
 - Compilation to real machine code - a real compiler is used: FreePascal/Lazarus
 - Open source

## Getting started
1. Install and run [Syncthing](https://syncthing.net/downloads/) on your computer. By default it listens on `http://127.0.0.1:8384` and has an API key in its settings.
2. Start SyncthingLazurite.
3. Open Options and **set Syncthing API key**.
   NOTE: the app can read the key from Syncthing’s config if you point it to the config folder (work in peogress).
4. Click "Connect". You should see your devices and folders populate within a few seconds.

## Feedback
Report issues and suggestions on the [issue tracker](https://github.com/heX16/SyncthingLazurite/issues).

## How to build

1. Download and install Lazarus.
2. Open the project.
3. Install missing packages via "Package → Online Package Manager...".
4. Lazarus will restart automatically after installing packages.
5. Click "Run → Build". That’s it!
   (With Lazarus you don’t need to open a console or manually edit build paths/configs — it works out of the box.)

## Roadmap

1. -[ ] Linux builds.
2. -[ ] macOS builds.
3. -[ ] Automatic Syncthing download if not found, plus a distribution that bundles Syncthing.
4. -[ ] Richer UI, closer to the Web UI.
5. -[ ] Automatic API key retrieval from the config.
6. -[ ] Automatic detection of configuration and existing Syncthing installation.
7. -[ ] Improve event log UI for recent activity.
8. -[ ] Support non-default ports and addresses for remote servers.
9. -[ ] Android experemental build.
