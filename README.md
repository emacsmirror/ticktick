# ticktick.el

ticktick.el enables two-way synchronization between [TickTick](https://ticktick.com), a popular task management app, and Emacs Org Mode.

## Features

- **Bidirectional sync**
- **OAuth2 authentication**
- **Preserves task metadata**: priorities, due dates, completion status, descriptions
- **Project-based organization** matching TickTick's structure
- **Automatic syncing** on focus changes or timer-based intervals

## Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/polhuang/ticktick.el.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/ticktick.el")
   (require 'ticktick)
   ```

## Setup

### 1. Register TickTick OAuth Application

1. Go to https://developer.ticktick.com/
2. Create a new OAuth application
3. Set the redirect URI to: `http://localhost:8080/ticktick-callback`
4. Note your Client ID and Client Secret

### 2. Configure Credentials

```elisp
(setq ticktick-client-id "your-client-id"
      ticktick-client-secret "your-client-secret")
```

Instead of hard-coding your client secret into your configuration, storing it with [auth-source](https://www.gnu.org/software/emacs/manual/html_mono/auth.html) is recommended.

### 3. Authorize Application

```
M-x ticktick-authorize
```

This will open your browser for OAuth consent and automatically capture the authorization.

### 4. Perform Initial Sync

```
M-x ticktick-sync
```

## Usage

### Main Commands

| Command | Description |
|---------|-------------|
| `ticktick-sync` | Full bidirectional sync |
| `ticktick-fetch-to-org` | Pull tasks from TickTick to Org |
| `ticktick-push-from-org` | Push Org tasks to TickTick |
| `ticktick-authorize` | Set up OAuth authentication |
| `ticktick-refresh-token` | Manually refresh auth token |
| `ticktick-toggle-sync-timer` | Toggle automatic timer-based syncing |

### Org File Structure

Tasks are stored in `~/.emacs.d/ticktick/ticktick.org` with this structure:

```org
* Project Name
:PROPERTIES:
:TICKTICK_PROJECT_ID: abc123
:END:
** TODO Task Title [#A]
DEADLINE: <2024-01-15 Mon>
:PROPERTIES:
:TICKTICK_ID: def456
:TICKTICK_ETAG: xyz789
:SYNC_CACHE: hash
:LAST_SYNCED: 2024-01-15T10:30:00+0000
:END:
Task description content here.
```

## Configuration

### Key Variables

```elisp
;; Path to the org file for tasks (default: ~/.emacs.d/ticktick/ticktick.org)
(setq ticktick-sync-file "/path/to/your/ticktick.org")

;; Directory for storing tokens and data (default: ~/.emacs.d/ticktick/)
(setq ticktick-dir "/path/to/ticktick/data/")

;; Enable automatic syncing on focus changes (default: nil)
(setq ticktick--autosync t)

;; Enable automatic syncing every N minutes (default: nil)
(setq ticktick-sync-interval 30)

;; Port for OAuth callback server (default: 8080)
(setq ticktick-httpd-port 8080)
```

### Automatic Syncing

Enable automatic syncing with one of these methods:

**Focus-based syncing** (syncs when switching buffers or losing focus):
```elisp
(setq ticktick--autosync t)
```

**Timer-based syncing** (syncs every N minutes):
```elisp
(setq ticktick-sync-interval 30)  ; Sync every 30 minutes
```

You can also toggle timer syncing interactively:
```
M-x ticktick-toggle-sync-timer
```

## Task Management

### Creating Tasks

Create tasks by just adding a `TODO` heading directly in your org file under any project heading:

```org
* Work Project
:PROPERTIES:
:TICKTICK_PROJECT_ID: project123
:END:
** TODO Review quarterly reports [#A]
DEADLINE: <2024-01-20 Sat>
Need to analyze Q4 performance metrics and prepare summary.
```

Run `M-x ticktick-push-from-org` to sync to TickTick.

### Task Priorities

Org priorities are mapped to TickTick priorities.

- `[#A]` - High priority
- `[#B]` - Normal priority  
- `[#C]` - Low priority
- No priority - Normal priority

### Task Status

- `TODO` - Open task
- `DONE` - Completed task

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

See [LICENSE](LICENSE) for full details.

## Acknowledgments

- Built on top of [request.el](https://github.com/tkf/emacs-request) library
- Uses [simple-httpd](https://github.com/skeeto/emacs-simple-httpd) for OAuth callbacks

