# ardentia

An Emacs view and command layer for an org-mode agent team. Tasks tagged
`:aiready:` are picked up by a background runner (headless AI agents),
run as `:ai_in_progress:` and end up as `:humanreview:` for you.
ardentia shows these queues, lets you review, accept or send back results
and creates new agent tasks.

Requirements: Emacs 30 or 31, Org 9.7 (built in), `transient` (built in).
No other Emacs packages.

License: GPL-3.0-or-later, see `LICENSE`.

## The backend is not included

ardentia is the front end only. The runner that claims and works the
tasks, and the `org-tool` program it reads through, are not part of this
repository. To use ardentia you need a backend that follows the contract
below; the test suite ships a fake `org-tool` that shows it in a few lines.

### Tag lifecycle

```
:aiready:          you tag a task as ready for the agents
:ai_in_progress:   the runner claims it (properties AI_CLAIMED, AI_CLAIMED_BY)
:humanreview:      the runner writes a run report and hands it back to you
```

Accept sets the task to DONE. Send back replaces `humanreview` by
`aiready` and adds your note. A task can be bound to one computer with a
tag `NAME_only`.

### What ardentia calls

- `org-tool find-tagged TAG FILE...` (`ardentia-org-tool`): prints a JSON
  array, one object per heading carrying TAG, with the fields `file`,
  `line`, `level`, `todo`, `title`, `tags` (array), `id`, `scheduled`,
  `deadline`, `created`, `done` (boolean) and `properties` (object). A
  missing value may be JSON `null` or the string `"null"`. Called for
  `humanreview`, `ai_in_progress` and `aiready` over the `*.org` files in
  `ardentia-org-directory`.
- `ardentia-host-command`: prints the name of the local computer.
- `ardentia-hosts-file`: lines `hosts=NAME NAME...` and
  `default_host=NAME`; other lines are ignored.
- `systemctl --user show` on `ardentia-service-unit` and
  `ardentia-timer-unit`: last and next run of the runner, shown in the
  dashboard. Set them to the units of your runner.

Properties ardentia shows when present: `AI_STATUS` (completed, blocked,
partial), `AI_SESSION`, `AI_CLAIMED`, `AI_CLAIMED_BY`,
`AI_LIMIT_INTERRUPTED`, `AI_ATTEMPTS`. A run report is an item at the top
of the `:LOGBOOK:` drawer that starts with `- AI-Lauf [TIMESTAMP]`; the
review buffer shows the newest one and the `- Note taken on` items above
it.

## Install

```elisp
(add-to-list 'load-path "/path/to/ardentia")
(require 'ardentia)
(global-set-key (kbd "C-c A") #'ardentia)
(ardentia-mode-line-mode 1)   ; optional: " AR:3" counter in the mode line
```

Loading the package has no side effects; the mode-line timer only runs
while `ardentia-mode-line-mode` is on.

## Commands

| Command                  | What it does                                         |
|--------------------------|------------------------------------------------------|
| `ardentia`               | Transient menu (d r n D g m)                         |
| `ardentia-dashboard`     | All queues in `*ardentia*`: review, running, ready   |
| `ardentia-review-inbox`  | Same view, only tasks waiting for review             |
| `ardentia-review-task`   | Read-only org view of one task (run report, notes, child sections) |
| `ardentia-new-task`      | Capture a new `:aiready:` task (asks for the host)   |
| `ardentia-delegate`      | Same, quoting the active region as data              |
| `ardentia-mode-line-mode`| Global minor mode with the review counter            |

### Keys in the dashboard

`g` refresh (asynchronous), `RET` open review, `o` visit the heading,
`a` accept, `s` send back, `q` quit, `?` menu.

### Keys in the review buffer

`a` accept, `s` send back (asks for a note), `y` let the agents do an open
child TODO themselves, `o` visit the heading, `g` re-render, `q` quit.
The buffer uses `org-indent-mode`; set `ardentia-review-indent` to nil to turn that off.

## Customization

`ardentia-org-directory`, `ardentia-org-tool`, `ardentia-host-command`,
`ardentia-hosts-file`, `ardentia-capture-file`,
`ardentia-mode-line-interval`, `ardentia-process-timeout`,
`ardentia-stale-minutes`,
`ardentia-timer-unit`, `ardentia-service-unit`.

## Design rules

- It is a view layer. Reading goes through `org-tool` in an external
  process, asynchronously; Emacs never blocks on a refresh.
- Mutations happen in the buffer that visits the org file, through org
  functions, like you would do them by hand. Files are never edited on
  disk behind Emacs' back. If the file changed on disk, an unmodified
  buffer is reverted; a modified one makes the action refuse.
- The buffer is saved after an action only when it was unmodified before.
  Otherwise it stays modified (the runner only sees saved state) and a
  message says so.
- Nothing is accepted automatically. `a` asks for confirmation.
- Send back replaces `humanreview` by `aiready` and writes your note as a
  normal logbook note ("Note taken on ...") in the format org uses.
- Quoted text (`ardentia-delegate`) is put into `#+begin_quote` blocks so
  the agents treat it as data, not as instructions.
- Host tags are `NAME_only`; the host list comes from
  `ardentia-hosts-file`, the local host from `ardentia-host-command`.

## Files

- `ardentia-core.el` parsing, queue logic, async fetch, shared state
- `ardentia-actions.el` mutations in the visiting buffer
- `ardentia-review.el` review buffer
- `ardentia-dashboard.el` dashboard and review inbox
- `ardentia-capture.el` new task and delegate
- `ardentia-modeline.el` mode-line counter
- `ardentia.el` entry point and transient menu
- `ardentia-test.el`, `run-tests` tests

## Tests

```sh
./run-tests
```

Byte-compiles everything with `byte-compile-error-on-warn` in a temporary
copy, then runs the ERT suite there in `emacs --batch -Q` with a temporary
`HOME`. The tests use temporary org files and a fake `org-tool`; they
never touch your real files.

Never leave a `.elc` next to the sources when the directory is shared
between computers with different Emacs versions: a stale `.elc` next to a
newer `.el` wins on load because `load-prefer-newer` is nil.
