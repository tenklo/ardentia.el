# ardentia

An Emacs view and command layer for an org-mode agent team. Tasks tagged
`:aiready:` are picked up by a background runner (headless AI agents),
run as `:ai_in_progress:` and end up as `:humanreview:` for you.
ardentia shows these queues, lets you review, accept or send back results
and creates new agent tasks.

Requirements: Emacs 30 or 31, Org 9.7 (built in), `transient` (built in).
No other Emacs packages; `agent-shell` is optional (see Session transcripts).

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
- `ardentia-run-task-command`: starts the runner on one task right now
  (`x`). In every element `%i` is the task's org ID and `%f` its file.
  The default uses `systemd-run --user` (a transient unit, independent
  of Emacs, output in the journal) and the runner's
  `ORG_AIREADY_ONLY_ID`: the runner waits for a running tick, then
  works exactly that task, also when it is scheduled for later. Any
  command that does the same will do; nil switches the feature off.
  Running asks for confirmation, refuses tasks bound to another host,
  and in an org buffer saves the file first, because the runner reads
  it from disk.

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
| `ardentia`               | Transient menu (d r n D g m, x in org buffers)       |
| `ardentia-dashboard`     | All queues in `*ardentia*`: review, running, ready, later |
| `ardentia-review-inbox`  | Same view, only tasks waiting for review             |
| `ardentia-review-task`   | Read-only org view of one task (run report, notes, child sections) |
| `ardentia-session-open` | Read-only, live-following transcript of the agent session of a task |
| `ardentia-session-open-agent-shell` | Resume that session in `agent-shell` (optional package) |
| `ardentia-new-task`      | Capture a new `:aiready:` task (asks for the host)   |
| `ardentia-delegate`      | Same, quoting the active region as data              |
| `ardentia-mode-line-mode`| Global minor mode with the review counter            |
| `ardentia-run-task-at-point` | Org buffer: start the runner now on the `:aiready:` task at point |
| `ardentia-dashboard-run` | Dashboard: start the runner now on the ready or later task at point |
| `ardentia-dashboard-priority` | Dashboard: set the priority of the ready or later task at point (SPC removes it) |
| `ardentia-dashboard-priority-up` / `-down` | Dashboard: raise or lower that priority |

### Keys in the dashboard

`g` refresh (asynchronous), `RET` open (review buffer for a review task,
session transcript for a running task, an error for other queues),
`t` session transcript of the task in any queue (the latest run),
`A` join the running session (see Session transcripts) or, for other tasks,
resume the latest session in `agent-shell`, `o` visit the heading,
`,` set the priority (SPC removes it), `+`/`-` (also `S-<up>`/`S-<down>`)
raise/lower it (ready and later tasks only),
(also on freshly captured tasks that have no `:ID:` yet: the heading is
found by its line or, failing that, by its unique title; `x` creates the
ID and saves the file first),
`a` accept, `s` send back, `l` hide or show later tasks, `x` run the ready
or later task now, `q` quit, `?` menu.

Queue `later`: the runner only picks up an `:aiready:` task once its
SCHEDULED time is due, so a ready task scheduled in the future is listed
as `later` instead of `ready`. It sorts after the ready tasks, soonest
due first, and the Flags column shows `due HH:MM` (due today) or
`due MM-DD`. Only the date and the first `HH:MM` after it count (the
start of a range; repeater and warning period are ignored). Without a
time, or with an impossible one (hour above 23, minute above 59), the
task is due from 00:00 that day; an unparseable timestamp counts as due.
Review and running tasks, the review inbox and the mode-line counter are
not affected. `ardentia-dashboard-show-later` (default t) sets whether
later tasks are listed; `l` toggles it in the current buffer.

Ready tasks are sorted by priority (`[#A]` first; no cookie counts as
`ardentia-default-priority`, B), then oldest first. The Title column
shows the cookie like org-agenda. The real order of the runner comes from
its own order configuration, where priority is one rule after a few
urgency rules, so the dashboard order is an approximation.

### Keys in the review buffer

`a` accept, `s` send back (asks for a note), `y` let the agents do an open
child TODO themselves, `o` visit the heading, `t` session transcript,
`g` re-render, `q` quit.
The buffer uses `org-indent-mode`; set `ardentia-review-indent` to nil to turn that off.

### Session transcripts

The runner starts one Claude Code session per task with the org directory
as working directory. Claude Code writes its transcript live to
`~/.claude/projects/SLUG/SESSION.jsonl`, where SLUG is the truename of
that directory with every character other than letters and digits
replaced by `-` (`/path/to/org` becomes `-path-to-org`). ardentia
finds the transcript of a task as the newest `*.jsonl` there whose first
16 KB contain `ID=<org id of the task>` (the prompt the runner sends).
For a running task only files modified after its `AI_CLAIMED` time
(minus 5 minutes) count.

`ardentia-session-mode` renders it as text: local time, prompts,
assistant text, tool calls as `> Bash: description`, tool results cut to
a few lines (`< ...`), failed results as `! ...` in the error face.
Messages you send into a running turn show up as prompts where the model
received them.
Subagent transcripts (`SESSION/subagents/agent-*.jsonl`) are merged in by
time stamp and prefixed with their type, for example `[qa]`; new ones
are picked up while following.

The buffer follows like `tail -f` every `ardentia-session-follow-interval`
seconds: only new bytes are read, a partial last line waits for the next
poll, and point stays at the end if it was there. The header line says
`following`, `not following` or `finished`. A session is finished when
the last refresh of the dashboard does not list the task as running, or
when neither the transcript nor one of its subagent transcripts was
written for `ardentia-session-idle-seconds` (default 900); a finished
buffer is no longer polled, `g` reads it again. The timer exists only while a
buffer needs it.

Keys: `g` re-read from scratch, `f` toggle following, `o` visit the
heading, `i` send a message into the running session, `a` open in
`agent-shell`, `q` quit.

Live input: the runner can keep an input pipe open per orchestrator, fed by
`ardentia-session-say-command` (default `ai/tools/aiready-say`;
`--check SESSION` exits 0 when the session accepts input, otherwise the
message on stdin is sent). `A` on a running task checks that; if the session
is live, its transcript buffer opens in the current window with `input: i`
in the header line, and no second process is started. Otherwise a message
says why and `A` resumes the session in `agent-shell` as before. `i` opens a
compose buffer (`C-c C-c` send, `C-c C-k` abort); the message arrives inside
the running turn and shows up in the transcript, which is followed.

`agent-shell` is an optional third-party package, not a dependency.
`ardentia-session-open-agent-shell` needs it installed (otherwise it
signals a `user-error`), calls `agent-shell-resume-session` with the
session id and the org directory as `default-directory`, and binds
`agent-shell-session-restore-verbosity` to
`ardentia-session-agent-shell-verbosity` (default `full`: the whole
conversation is replayed). That starts a new agent process on the
session, so it asks for confirmation when the task is still running.
`ardentia-session-viewer` (`ardentia` or `agent-shell`) chooses which
viewer RET uses on a running task.

## Customization

`ardentia-org-directory`, `ardentia-org-tool`, `ardentia-host-command`,
`ardentia-hosts-file`, `ardentia-capture-file`,
`ardentia-mode-line-interval`, `ardentia-process-timeout`,
`ardentia-stale-minutes`, `ardentia-dashboard-show-later`,
`ardentia-timer-unit`, `ardentia-service-unit`,
`ardentia-run-task-command`,
`ardentia-session-directory`, `ardentia-session-viewer`,
`ardentia-session-say-command`,
`ardentia-session-agent-shell-verbosity`,
`ardentia-session-follow-interval`, `ardentia-session-result-lines`,
`ardentia-session-line-width`, `ardentia-session-idle-seconds`,
`ardentia-session-search-limit`.

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
- `ardentia-session.el` session transcript viewer, optional agent-shell
- `ardentia-dashboard.el` dashboard and review inbox
- `ardentia-capture.el` new task and delegate
- `ardentia-run.el` start the runner on one task now
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
