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

Three more tags mark trouble, in any lifecycle state or none:

```
:auto_error:       a run or an automatic step failed
:syncmerge:        a file sync conflict needs a merge
:gitsync:          a git sync needs attention
```

### What ardentia calls

- `org-tool find-tagged TAG FILE...` (`ardentia-org-tool`): prints a JSON
  array, one object per heading carrying TAG, with the fields `file`,
  `line`, `level`, `todo`, `title`, `tags` (array), `id`, `scheduled`,
  `deadline`, `created`, `done` (boolean) and `properties` (object). A
  missing value may be JSON `null` or the string `"null"`. Called once
  with `aiready|ai_in_progress|humanreview|auto_error|syncmerge|gitsync` (TAG may be several tags
  joined by `|`; a heading carrying more than one is listed once) over
  the `*.org` files in `ardentia-org-directory`. If that call exits
  non-zero, ardentia falls back to one call per tag, so a program that
  only knows a single TAG keeps working.
- `ardentia-host-command`: prints the name of the local computer.
- `ardentia-hosts-file`: lines `hosts=NAME NAME...` and
  `default_host=NAME`; other lines are ignored.
- `systemctl --user show` on `ardentia-service-unit` and
  `ardentia-timer-unit`: last and next run of the runner, shown in the
  dashboard. Set them to the units of your runner.
- `ardentia-review-alert-skip-command`: a program that decides whether
  a task should not be announced. Only called when
  `ardentia-review-alert` is on, as `PROGRAM FILE ID` for each new
  review task that has an ID. Exit status 0 means do not announce the
  task; any other status, or a program that is missing or not
  executable, means announce it. nil switches the check off.
- `ardentia-run-task-command`: starts the runner on one task right now
  (`x`). In every element `%i` is the task's org ID and `%f` its file.
  The default uses `systemd-run --user` (a transient unit, independent
  of Emacs, output in the journal) and the runner's
  `ORG_AIREADY_ONLY_ID`: the runner works exactly that task at once,
  in parallel to a running tick if there is one, also when the task is
  scheduled for later. A second request for the same task while it
  runs is ignored. Any command that does the same will do; nil
  switches the feature off.
  Running asks for confirmation, refuses tasks bound to another host,
  and in an org buffer saves the file first, because the runner reads
  it from disk.
- `ardentia-stop-task-command`: stops the run of one running task
  (`k`). In every element `%i` is the task's org ID and `%f` its file.
  The default is `ai/tools/aiready-stop ID` below
  `ardentia-org-directory`. The contract: the command signals the
  agent process working on that task on this computer, the runner then
  closes the task (blocked, back to review), and the command prints one
  line of text. Exit codes: 0 stopped and the task is closed, 3 the task
  is not running on this computer, 4 the signal was sent but the runner
  has not finished closing the task yet, 1 usage error, 2 any other
  failure. Only the computer that claimed the task
  (`AI_CLAIMED_BY`) can stop it. Nil switches the feature off.
  Stopping asks for confirmation, refuses tasks that are not running
  and tasks claimed by another host, and never blocks Emacs; the
  dashboards refresh after exit 0 and 4.

Properties ardentia shows when present: `AI_STATUS` (completed, blocked,
partial), `AI_SESSION` (`YYYYMMDD-HHMM`, when the work finished),
`AI_CLAIMED`, `AI_CLAIMED_BY`, `AI_LIMIT_INTERRUPTED`, `AI_ATTEMPTS`.
The usage properties are the runner's contract; the runner writes them
(as strings) after every run: `AI_TOKENS` (tokens of the last run, an
integer), `AI_TOKENS_TOTAL` (sum over all runs), `AI_COST_USD` (cost of
the last run in US dollars, for example `2.19`), `AI_COST_USD_TOTAL`
(sum over all runs) and `AI_USAGE_RUNS` (number of runs). Tasks that
were finished before the runner wrote them simply have none; ardentia
then shows empty cells, never 0. A run report is an item at the top
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
| `ardentia`               | Transient menu (d r c k a n D g m, x in org buffers)  |
| `ardentia-dashboard`     | Task list `*ardentia*` in its dashboard view: trouble, review, running, ready, later |
| `ardentia-review-inbox`  | The same buffer in its review view: only tasks waiting for review |
| `ardentia-completed`     | The same buffer in its completed view: all tasks handed back, accepted or not, with tokens and cost |
| `ardentia-knowledge`     | Read-only list of the knowledge files in `*ardentia knowledge*`; large files are grepped, not opened |
| `ardentia-agents`        | Read-only list of the agent definitions with model, tools and limits in `*ardentia agents*` |
| `ardentia-review-task`   | Read-only org view of one task (run report, notes, child sections) |
| `ardentia-session-open` | Read-only, live-following transcript of the agent session of a task |
| `ardentia-session-open-agent-shell` | Resume that session in `agent-shell` (optional package) |
| `ardentia-new-task`      | Capture a new `:aiready:` task (asks for the host)   |
| `ardentia-delegate`      | Same, quoting the active region as data              |
| `ardentia-mode-line-mode`| Global minor mode with the review counter            |
| `ardentia-run-task-at-point` | Org buffer: start the runner now on the `:aiready:` task at point |
| `ardentia-dashboard-run` | Dashboard: start the runner now on the ready or later task at point |
| `ardentia-stop-task` | Stop the run of a running task (asks first; the claiming computer only) |
| `ardentia-dashboard-stop` | Dashboard: stop the running task at point |
| `ardentia-session-stop` | Session transcript: stop the task of this transcript |
| `ardentia-dashboard-priority` | Dashboard: set the priority of the ready or later task at point (SPC removes it) |
| `ardentia-dashboard-priority-up` / `-down` | Dashboard: raise or lower that priority |
| `ardentia-dashboard-menu`, `ardentia-review-menu`, `ardentia-knowledge-menu`, `ardentia-agents-menu`, `ardentia-session-menu` | `?` in the respective buffer: transient menu listing every key of that buffer (the task list: groups View and Filter among them), grouped, under the same key; `.` opens `ardentia` (also directly in every buffer) |

### Keys in the task list

There is one task list buffer, `*ardentia*`. `ardentia-dashboard`,
`ardentia-review-inbox` and `ardentia-completed` all show that buffer and
switch it to a view; filters stay set across views.

Views (all views):
`1` dashboard (all open queues), `2` review (tasks waiting for review),
`3` completed (everything handed back, see below), `TAB` / `<backtab>` next /
previous view (cyclic), `c` and `r` are aliases for completed and review.
The header line starts with the views, the active one in brackets. Each view
remembers the row you were on.

Filters (they show in the header line while not at their default):
- `h` host: cycles all, this computer, the other hosts of `ardentia-hosts`
- `/` title regexp (case-insensitive; empty input clears it)
- `l` later tasks shown or hidden (dashboard view only)
- `P` period: all, last 30 days, last 7 days, today (completed view only;
  by `AI_SESSION`, so tasks without it only show under all)
- `w` state: all, waiting for review, accepted (completed view only)
- `f` transient with all filters, their current values and `c` to clear them
  all; filters that do not apply to the view are greyed out
- a filter key in the wrong view signals an error naming the view it belongs to
- `n` / `p` stay line navigation

Other keys:
`g` refresh (asynchronous; visible task lists also refresh by themselves every
`ardentia-dashboard-refresh-interval` seconds; the completed view fetches its
data again each time), `RET` open (review buffer for a review task,
session transcript for a running task, an error for other queues),
`t` session transcript of the task in any queue (the latest run),
`A` join the running session (see Session transcripts) or, for other tasks,
resume the latest session in `agent-shell`, `o` visit the heading,
`,` set the priority (SPC removes it), `+`/`-` (also `S-<up>`/`S-<down>`)
raise/lower it (ready and later tasks only),
(also on freshly captured tasks that have no `:ID:` yet: the heading is
found by its line or, failing that, by its unique title; `x` creates the
ID and saves the file first),
`a` accept, `s` send back (both refuse a task that is already accepted),
`d` toggle the due flag between time left and due date, `x` run the ready
or later task now, `k` stop the running task at point, `u` fetch the usage now,
`S` sort by column, `q` quit, `?` menu of all keys of this buffer
(`ardentia-dashboard-menu`, grouped, same keys; `.` opens the global menu, as it does directly in the buffer).
A refresh keeps point and scroll position on the same task.
The Cost column shows `AI_COST_USD_TOTAL` for every task that has it.
For a review task, the Flags column ends with the class of the `For you`
line of its newest run report: `you!` when the line contains `REQUIRED`,
`opt` when it only offers optional follow-ups, nothing for `nothing to do`
or a report without the line. The report is read from the task's buffer
once per run (cached by file, ID and `AI_SESSION`); a read error shows no
flag.

Queue `trouble`: what needs attention beyond the normal review. It
lists every open task tagged `:auto_error:`, `:syncmerge:` or
`:gitsync:` and every running merge, above all other queues, oldest
first. The Flags column of a task starts with `KIND:STATE`, KIND being
`error`, `merge` or `gitsync` and STATE the lifecycle (`review`,
`running`, `ready`, `later`; just KIND without a lifecycle tag). Such a
task keeps its normal behaviour: it still counts as a review task for
the review inbox and the mode-line counter when it is tagged
`:humanreview:`, and later tasks of this group are not hidden by `l`.
A running merge is a subdirectory of `ai/conflicts/work/` in
`ardentia-org-directory` (one per merge; plain files there are not
merges). Its row shows `trouble`, the flag `merging`, the age of the
directory, the name of the synced file and `Running merge: DIRECTORY`.
`RET` or `o` on it opens the directory in dired; every other command
signals "Not a task: a running merge". The group is absent when there is
nothing in it, and the review inbox never lists merges.

Queue `later`: the runner only picks up an `:aiready:` task once its
SCHEDULED time is due, so a ready task scheduled in the future is listed
as `later` instead of `ready`. It sorts after the ready tasks, soonest
due first, and the Flags column shows `due in 3h05m` (time until due: `12m`, `3h05m`,
`2d3h`, minutes rounded up) or, with `absolute`, `due HH:MM` (due today) or
`due MM-DD`. Only the date and the first `HH:MM` after it count (the
start of a range; repeater and warning period are ignored). Without a
time, or with an impossible one (hour above 23, minute above 59), the
task is due from 00:00 that day; an unparseable timestamp counts as due.
Review and running tasks, the review inbox and the mode-line counter are
not affected. `ardentia-dashboard-show-later` (default t) sets whether
later tasks are listed; `l` toggles it in the current buffer.
`ardentia-due-display` (default `relative`, or `absolute`) sets the form of
the due flag; `d` toggles it in the current buffer.

Ready tasks are sorted by priority (`[#A]` first; no cookie counts as
`ardentia-default-priority`, B), then oldest first. The Title column
shows the cookie like org-agenda. The real order of the runner comes from
its own order configuration, where priority is one rule after a few
urgency rules, so the dashboard order is an approximation.

### Usage limits and runner pause

The header line also shows the current Claude usage and, when the runner
is waiting for its limit to reset, the pause:

```
... | 5h 42% (resets 14:00) | week 7% (resets Mon 04:00) | SomeModel week 30% | runner paused until 14:00
```

- Source: `GET https://api.anthropic.com/api/oauth/usage`, the endpoint
  behind Claude Code's `/usage`. It is undocumented and may change or
  disappear; every field is treated as optional.
- The OAuth token is read locally from `~/.claude/.credentials.json`
  (`claudeAiOauth.accessToken`), kept in a local variable and sent only to
  that URL. It is handed to `curl` on standard input, so it never shows
  up in a process list, and it is never logged.
- The generic `limits` list is preferred (session = `5h`, `weekly_all` =
  `week`, per-model weekly limits as `<model> week`); without it the
  `five_hour` and `seven_day` fields are used. Per-model limits at 0% are
  left out (`ardentia-usage-hide-idle-scoped`). Extra usage appears only
  when enabled. Reset times are local: `HH:MM` today, otherwise `Ddd HH:MM`.
- A limit at or above `ardentia-usage-warn-percent` (or with a severity
  other than normal) uses the `warning` face, at 100% or critical the
  `error` face. Data older than twice the fetch interval gets
  `(stale 12m)`.
- The fetch is asynchronous, runs from the dashboard refresh but is
  throttled to one per `ardentia-usage-min-interval` seconds (default
  300); in between the cached value is shown. It is not part of the task
  list refresh, so a slow network delays nothing. `u` forces a fetch.
- A failure never raises an error; the line says `usage: unavailable
  (REASON)` (no network, HTTP status, token expired: run `claude` once to
  refresh the login, no credentials, curl missing).
- Runner pause: when the runner hit the session limit it writes
  `${XDG_STATE_HOME:-~/.local/state}/process-aiready/limit-until`, one line
  `UNTIL_EPOCH<TAB>TEXT`. While that time lies ahead the line shows
  `runner paused until HH:MM`. A missing, expired or malformed file means
  no pause. The path is `ardentia-usage-limit-state-file`.
- `ardentia-usage-enabled` nil turns the fetch and the usage text off.
- Narrow windows: when the line does not fit, the timer times are
  shortened (`HH:MM` today, otherwise `Ddd HH:MM`) and the header line
  keeps as many parts as fit. The remaining parts move into lines at the
  top of the buffer, and the layout follows window resizes.

### The completed view

The completed view (`3`, `c` in the menu, `ardentia-completed`) lists every
heading tagged `:humanreview:`, still waiting for review as well as
accepted (DONE, CANC; accepting keeps the tag). It fetches
asynchronously with its own `org-tool find-tagged humanreview` call when you switch to it and on every refresh;
a failed fetch keeps the old list and shows the error in the header line. The columns are Finished (`AI_SESSION`),
State (the TODO keyword: DONE or CANC means accepted), Status
(`AI_STATUS`), Host (`AI_CLAIMED_BY`), Runs (`AI_USAGE_RUNS`), Tokens
(`AI_TOKENS_TOTAL`, as 950, 12.3k, 5.4M), Cost (`AI_COST_USD_TOTAL`),
File and Title. Click a header (or `S`) to sort; Runs, Tokens and Cost
sort numerically. The default order is newest Finished first, tasks
without `AI_SESSION` last. Missing usage data gives empty cells.

The header line shows the number of tasks shown with the split into
completed, blocked and partial, the summed tokens and cost with
"(N with usage data)" (only tasks that carry usage properties add to the
sums), the period, the time of the last refresh and an error if any.

### Knowledge and agent browsers

Both browsers are strictly read-only and read only the head of each file
to build their lists.

`ardentia-knowledge` (`k` in the menu) lists the regular files of
`ardentia-knowledge-directory` (not recursive; dotfiles and backups are
skipped) with Name, Size, Modified and the title (the first `# ` heading).
Files larger than `ardentia-knowledge-large-file-size` are marked "grep
only" and are never visited whole by `RET`. Keys: `RET` view a small file
read-only, or ask for a regexp and grep that one large file in a
`grep-mode` buffer; `s` grep a regexp over all files; `o` visit the file
(a large one only after a yes-or-no question); `g` refresh; `q` quit; `?`
menu of all keys of this buffer (`ardentia-knowledge-menu`; `.` opens the
global menu, as it does directly in the buffer).

`ardentia-agents` (`a` in the menu) lists the `*.md` files of
`ardentia-agents-directory` with the simple `key: value` lines of their
YAML frontmatter: Name, Model, Tools, Permission (`permissionMode`), Turns
(`maxTurns`), OmitMd (`omitClaudeMd`) and Description. Missing keys show
"-"; a file without frontmatter shows its file name. Keys: `RET` view the
file read-only, `g` refresh, `q` quit, `?` menu of all keys of this buffer
(`ardentia-agents-menu`; `.` opens the global menu, as it does directly in the buffer).

### Keys in the review buffer

`a` accept, `s` send back (asks for a note), `y` let the agents do an open
child TODO themselves, `o` visit the heading, `t` session transcript,
`g` re-render, `q` quit, `?` menu of all keys of this buffer
(`ardentia-review-menu`; `.` opens the global menu, as it does directly in the buffer).
The headline carries the TODO keyword and priority, and the
`- Status ::` line shows the keyword and the agents' `AI_STATUS`.
When the usage properties exist, the section "Agent properties" starts
with one line such as `- Usage :: 5.4M tokens, $4.09 over 2 runs (last
run 3.0M tokens, $2.19)`.
When the newest run report opens with a `- For you:` line (the first
sub-item right after the `- AI-Lauf [...]` line; older reports are never
consulted), the header shows it as `- For you :: TEXT`, with the
deeper-indented sub-items of the multi-item form below it. Org links stay
clickable. Without such a line nothing is added.
The buffer uses `org-indent-mode`; set `ardentia-review-indent` to nil to turn that off.
Long lines wrap by default, independent of `org-startup-truncated`; set `ardentia-review-truncate-lines` to t to truncate them instead, or press `C-x x t` (`toggle-truncate-lines`) in one buffer.

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
heading, `i` send a message into the running session, `k` stop the running
task, `a` open in
`agent-shell`, `C-c C-n`/`C-c C-p` (also `n`/`p`) next/previous entry,
`q` quit, `?` menu of all keys of this buffer (`ardentia-session-menu`; `.`
opens the global menu, as it does directly in the buffer).

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
`ardentia-stale-minutes`, `ardentia-dashboard-show-later`, `ardentia-due-display`,
`ardentia-dashboard-colors` (default on, see Colors below),
`ardentia-completed-period` (default `all`; also `last-30-days`,
`last-7-days`, `today`),
`ardentia-dashboard-refresh-interval` (default 60 seconds, nil or 0 turns the
automatic refresh off; only dashboards shown on a visible frame are refreshed,
never while the minibuffer is active or a refresh runs; re-open the dashboard
to restart the timer),
`ardentia-timer-unit`, `ardentia-service-unit`,
`ardentia-run-task-command`, `ardentia-stop-task-command`,
`ardentia-knowledge-directory`, `ardentia-knowledge-large-file-size`
(default 100000 bytes), `ardentia-agents-directory`,
`ardentia-session-directory`, `ardentia-session-viewer`,
`ardentia-session-say-command`,
`ardentia-session-agent-shell-verbosity`,
`ardentia-session-follow-interval`, `ardentia-session-result-lines`,
`ardentia-session-line-width`, `ardentia-session-idle-seconds`,
`ardentia-session-search-limit`,
`ardentia-usage-enabled`, `ardentia-usage-credentials-file`,
`ardentia-usage-url`, `ardentia-usage-min-interval`,
`ardentia-usage-limit-state-file`, `ardentia-usage-warn-percent`,
`ardentia-usage-hide-idle-scoped`,
`ardentia-review-alert`, `ardentia-review-alert-ding`,
`ardentia-review-alert-skip-command`.

### Alert for new review tasks

`ardentia-review-alert` is off (nil) by default. Set it to `message` to
get an echo-area message when a task newly reaches the review queue, or
to a function that receives the list of the new task plists (for
example a wrapper around `alert` or `notifications-notify`).
`ardentia-review-alert-ding` also rings the bell.

- The first observation after loading is only a baseline: tasks that
  are already waiting are not announced.
- A task that leaves the queue and comes back is announced again.
- Several new tasks are announced together in one alert.
- New tasks are first run through `ardentia-review-alert-skip-command`,
  see "What ardentia calls".
- There is no timer of its own. The check runs with the refresh of the
  mode-line counter (`ardentia-mode-line-mode`) and of an open
  dashboard, so alerts only arrive while one of them is active.
- Other code can use the abnormal hook `ardentia-review-tasks-functions`,
  which receives the current review tasks after each such refresh.

### Colors

The dashboard colors a few columns, subtly. The faces (customize group
`ardentia-faces`) inherit theme faces, so they follow light and dark
themes:

- `ardentia-queue-trouble` (error), `ardentia-queue-review` (keyword),
  `ardentia-queue-running` (string), `ardentia-queue-ready` (default),
  `ardentia-queue-later` (shadow): the Queue column
- `ardentia-status-completed` (success), `ardentia-status-partial`
  (warning), `ardentia-status-blocked` (error): the Status column
- `ardentia-priority-highest` (warning) and `ardentia-priority`
  (constant): the `[#X]` token at the start of the title, the first for
  `org-priority-highest`
- `ardentia-flag-attention` (warning): `you!`, `limit`, `stale?` and an
  attempts count like `3!`
- `ardentia-flag-trouble` (error): the trouble kind, `merging` and the
  ERROR text in the header line
- `ardentia-flag-info` (shadow): `opt`, the due flag and `refreshing...`
- `ardentia-dashboard-meta` (shadow): Host, Age, Cost and File columns

Change them with `M-x customize-group RET ardentia-faces RET`, or with
`set-face-attribute` or `custom-set-faces`. Set
`ardentia-dashboard-colors` to nil for the plain look; it applies at the
next refresh.

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
- `ardentia-dashboard.el` the task list buffer with its dashboard, review and completed views, filters
- `ardentia-completed.el` data side of the completed view: fetch, period, state, sorting, totals
- `ardentia-browse.el` read-only browsers for knowledge files and agents
- `ardentia-capture.el` new task and delegate
- `ardentia-run.el` start the runner on one task now
- `ardentia-usage.el` usage limits and runner pause in the header line
- `ardentia-modeline.el` mode-line counter
- `ardentia-alert.el` opt-in alert for new review tasks
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
