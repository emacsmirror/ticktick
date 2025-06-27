# ticktick.el

`ticktick.el` is an emacs package that syncs tasks between [TickTick](https://ticktick.com) and Org-mode.

## Features

- One or two-way sync between TickTick and an Org file
- Converts TickTick tasks into Org-mode headings with TODO state, priority, deadline, and content
- Pushes modified Org headings back to TickTick
- OAuth2 authentication
- Auto-sync on buffer / focus switch

## Requirements

- emacs 27+
- `org`, `json`, `oauth2` libraries

## Getting Started

### 1. Clone this repo

```sh
git clone https://github.com/polhuang/ticktick.el
````

### 2. Load it in your config

```elisp
(add-to-list 'load-path "~/.../ticktick")
(require 'ticktick)
```

### 3. Register your TickTick OAuth app

Go to [TickTick Developer Console](https://developer.ticktick.com/) and register a new application. The `Oauth redirect URI` must match the custom variable `ticktick-redirect-uri` exactly (default is `http://localhost`). 

Then in your emacs config, set the credentials provided by the TickTick Developer Console:

```elisp
(setq ticktick-client-id "your-client-id")
(setq ticktick-client-secret "your-client-secret")
```

### 4. Authorize access

Run:

```elisp
M-x ticktick-authorize
```

An authorization page will pop up in your browser. Extract the code parameter from the URL and enter it into the minibuffer prompt.

### 5. Sync

Configure where TickTick tasks should be synced:

```elisp
(setq ticktick-sync-file "~/.../tasks.org")
```

Fetch all tasks from TickTick into an Org file:

```elisp
M-x ticktick-fetch-to-org
```

Push any local changes back to TickTick:

```elisp
M-x ticktick-push-from-org
```

Or run a full two-way sync:

```elisp
M-x ticktick-sync-two-way
```

## Org Structure

Fetched tasks will be placed under project-level headings like:

```org
* Work
:PROPERTIES:
:TICKTICK_PROJECT_ID: bd974ee78bc82a7759515e5a
:END:

** TODO Submit invoice
DEADLINE: <2025-06-30 Mon>
:PROPERTIES:
:TICKTICK_ID: abc123
:TICKTICK_ETAG: 1a2b3c
:LAST_SYNCED: 2025-06-25T12:00:00-0500
:SYNC_CACHE: abcdef0123456789...
:END:
Attach PDF and update totals
```

TickTick task statuses are mapped to Org-mode TODO states.
* `TODO` → TickTick status 0 (not completed)
* `DONE` → TickTick status 2 (completed)

TickTick task priorities are mapped to Org-mode priorities:
  * `[#A]` → TickTick priority `5` (High)
  * `[#B]` → TickTick priority `3` (Medium)
  * `[#C]` → TickTick priority `1` (Low)
  * No priority → TickTick priority `0` (None)

## Autosync

To enable autosync (on buffer or window switch):

```elisp
(setq ticktick--autosync t)
```

## Notes

* Syncing uses ETag and SHA1 hash-based change detection to avoid duplicates. Do not edit them.
* Tasks can include `DEADLINE`, priority (`[#A]`, `[#B]`, etc.), and freeform content.

## Roadmap

- [x] Oauth2 authentication
- [x] Fetch all tasks from TickTick
- [x] Push all tasks to TickTick
- [x] Two-way sync
- [ ] Transient menu
- [ ] Delete task at point from TickTick
- [ ] Fetch one task from TickTick
- [ ] Push task at point to TickTick
- [ ] Sync task at point
- [ ] Dry run mode

