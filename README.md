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

Accept sets the task to DONE. When org refuses DONE (an open child TODO
under `org-enforce-todo-dependencies`, open checkboxes), accept says so,
names the blocker and leaves the task and the review buffer as they
were; a bulk accept counts such a task as skipped. Send back replaces `humanreview` by
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
it. Child headings without a TODO keyword (result sections) are shown
with their content under `Result sections`: drawers and planning lines
are left out and their sub-headings nest below the section.

## Install

```elisp
(add-to-list 'load-path "/path/to/ardentia")
(require 'ardentia)
(global-set-key (kbd "C-c A") #'ardentia)
(ardentia-mode-line-mode 1)   ; optional: " AR:3" counter in the mode line
```

Loading the package has no side effects; the mode-line timer only runs
while `ardentia-mode-line-mode` is on.

### Reloading after an update

`M-x ardentia-reload` loads all ardentia modules again from the directory
the package was loaded from (or from the directory given as argument), so
an update takes effect without restarting Emacs.

- Every `ardentia*.el` there is loaded, the `.elc` when it is not older
  than the `.el`, dependencies first; new modules need no configuration.
- Option and variable values stay, including values set with `setq`.
- Open task list, review and transcript buffers keep working and get the
  new keys at once: the keymap objects they use are updated in place.
  Key bindings of your own on ardentia maps survive when they are made in
  `with-eval-after-load`, which runs again after each load.
- Running timers (auto-refresh, mode-line counter) keep running, without
  duplicates.
- It never prompts, so it can run unattended after a `git pull`.  On a
  failure it signals an error naming the module and leaves the old keymaps
  in place.

Some changes still need a restart: new default values of options and
variables that are already set (the current values win), changed face
defaults, and changed syntax or abbrev tables of a mode.  A key you bind
on an ardentia map outside `with-eval-after-load` is lost on reload.

## Commands

| Command                  | What it does                                         |
|--------------------------|------------------------------------------------------|
| `ardentia`               | Transient menu (d r c s k a n D g m, x M in org buffers) |
| `ardentia-dashboard`     | Task list `*ardentia*` in its dashboard view: trouble, review, running, ready, later |
| `ardentia-review-inbox`  | The same buffer in its review view: only tasks waiting for review |
| `ardentia-completed`     | The same buffer in its completed view: all tasks handed back, accepted or not, with tokens and cost |
| `ardentia-knowledge`     | Read-only list of the knowledge files in `*ardentia knowledge*`; large files are grepped, not opened |
| `ardentia-stats`         | Read-only agent stats dashboards in `*ardentia-stats*`, rendered from the JSON file of an external tool (see Agent stats) |
| `ardentia-agents`        | Read-only list of the agent definitions with model, tools and limits in `*ardentia agents*` |
| `ardentia-review-task`   | Read-only org view of one task (run report, notes, result sections with their content, open child tasks) |
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
| `ardentia-set-model` | Org buffer: ask for a role (orchestrator, planner, researcher, worker, qa), then set or remove its `AI_MODEL_<ROLE>` on the heading at point (`AI_MODEL_ORCHESTRATOR` is the model the runner starts the task's orchestrator with); offers the aliases and the model IDs available to your login, `C-u` refetches; `org-set-property` completes the same values |
| `ardentia-dashboard-set-model` | Dashboard (`M`): the same for the task at point |
| `ardentia-dashboard-priority` | Dashboard: set the priority of the task at point (SPC removes it) |
| `ardentia-dashboard-priority-up` / `-down` | Dashboard: raise or lower that priority |
| `ardentia-dashboard-mark`, `-unmark`, `-unmark-all`, `-mark-toggle`, `-mark-all`, `-mark-toggle-all`, `-mark-regexp` | Dashboard: marks, like org-agenda's bulk marks |
| `ardentia-dashboard-fold-toggle`, `-fold-all` | Dashboard: fold or unfold the group of review tasks at point (`TAB`), or all groups (`<backtab>`) |
| `ardentia-dashboard-set-tags`, `-schedule`, `-deadline`, `-todo` | Dashboard: org-agenda's tag, schedule, deadline and TODO state commands on the task at point |
| `ardentia-dashboard-bulk-action` | Dashboard: apply tag, schedule, deadline, TODO state or priority to all marked tasks, or accept the marked review tasks |
| `ardentia-review-set-tags`, `-schedule`, `-deadline`, `-todo`, `-priority`, `-priority-up`, `-priority-down` | Review buffer: the same edits on the reviewed task |
| `ardentia-dashboard-menu`, `ardentia-review-menu`, `ardentia-knowledge-menu`, `ardentia-agents-menu`, `ardentia-session-menu`, `ardentia-stats-menu` | `?` in the respective buffer: transient menu listing every key of that buffer (the task list: groups View and Filter among them), grouped, under the same key; `.` opens `ardentia` (also directly in every buffer) |

### Keys in the task list

There is one task list buffer, `*ardentia*`. `ardentia-dashboard`,
`ardentia-review-inbox` and `ardentia-completed` all show that buffer and
switch it to a view; filters stay set across views.

Views (all views):
`1` dashboard (all open queues), `2` review (tasks waiting for review),
`3` completed (everything handed back, see below), `]` / `[` next /
previous view (cyclic), `c` and `r` are aliases for completed and review.
`4` opens the agent stats (`ardentia-stats`), a buffer of its own that is
not part of the `]` / `[` cycle.
The header line starts with the views, the active one in brackets. Each view
remembers the row you were on.

Filters (they show in the header line while not at their default):
- `h` host: cycles all, this computer, the other hosts of `ardentia-hosts`
- `/` title regexp (case-insensitive; empty input clears it)
- `\` tags: completes over the tags in the list; `tag` (or `+tag`) must be
  carried, `-tag` must not be (all must hold, exact and case-sensitive; empty
  input clears it); running merges have no tags and are hidden while a tag is
  required
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
raise/lower it (every task row),
(also on freshly captured tasks that have no `:ID:` yet: the heading is
found by its line or, failing that, by its unique title; `x` creates the
ID and saves the file first),
`a` accept (the marked tasks when any are marked, like `B a`), `s` send back (both refuse a task that is already accepted),
`d` toggle the due flag between time left and due date, `x` run the ready
or later task now, `k` stop the running task at point, `$` fetch the usage now,
`S` sort by column, `q` quit, `?` menu of all keys of this buffer
(`ardentia-dashboard-menu`, grouped, same keys; `.` opens the global menu, as it does directly in the buffer).
A refresh keeps point and scroll position on the same task.

Review tasks are grouped under their parent heading (dashboard and review view; the
completed view and the other queues stay flat):
- a parent heading with two or more listed review tasks gets a header row
  (`▾ Title (3)`, the outline path in the tooltip) and its tasks follow, indented
  - a review task whose parent heading is itself a listed review task is nested
    under that task row, which then folds like a header
  - a parent with one listed review task leaves it flat
    (`ardentia-dashboard-group-min`, default 2); `ardentia-dashboard-group-reviews`
    nil switches grouping off; sorting a column shows the rows flat meanwhile
  - the group stands where its first member stands, the members keep their order
- `TAB` folds or unfolds the group at point (on a task: folds its group and goes to
  the header), `<backtab>` folds all groups, or unfolds all when all are folded, RET on a
  header does the same as `TAB`
  - folds are kept across refreshes and view switches
  - header rows are no tasks: they cannot be marked or acted on; a mark on a
    folded task stays, `*` marks only the rows shown, bulk actions include
    the marked folded tasks
  - needs an org-tool whose `find-tagged` reports `parent` and `outline`; an older
    one shows the review tasks flat

Marks and edits work like in org-agenda:
- `m` marks the task and moves down, `u` unmarks and moves down, `U`
  unmarks all, `M-m` toggles the row, `*` marks all rows shown, `M-*`
  toggles all rows, `%` marks the tasks whose title matches a regexp
  - marked rows show `>`, the header line shows `N marked`
  - marks are kept across `g`, the automatic refresh and view switches; a
    task that is not listed any more loses its mark; a running merge cannot
    be marked
- `:` or `C-c C-q` set the tags, `C-c C-s` schedules, `C-c C-d` sets the
  deadline (`C-u` removes either), `C-c C-t` changes the TODO state, all on
  the task at point (the marks are not used), on the real heading
  - a log note that org asks for is stored and the file saved afterwards
  - the heading must still carry the lifecycle tag the row was listed under
- `B` applies an action to the marked tasks: `+` add a tag, `-` remove a
  tag, `s` schedule, `d` deadline, `t` TODO state, `,` priority, `a` accept
  - `a` sets DONE on the marked tasks that wait for review after one
    confirmation naming the count; other marked tasks are skipped ("not
    waiting for review"); a declined confirmation keeps the marks
  - what the action needs is asked for once; `C-u B` with `s` or `d` removes
  - every file is saved once, the lists are refreshed once
  - a task that cannot be changed is skipped; the message reads
    `Bulk: N done, M skipped` plus the first reason
  - no log entries are written in a bulk action
  - the marks are cleared afterwards unless `org-agenda-persistent-marks` is non-nil
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

### Agent stats

`ardentia-stats` (`s` in the menu, `4` in the task list) shows dashboards
about what the agent team does in the read-only buffer `*ardentia-stats*`.
Ardentia only renders them: an external tool, not part of this package,
computes the numbers and writes them into one JSON file,
`ardentia-stats-file` (default `~/.cache/agent-stats/summary.json`). Every
dashboard is a generic table, so a new dashboard needs no change here.

The header line shows the window of the data (days and weeks, when the
file has a `view` key), when the file was generated and how long ago, and
a key hint. The note of the file comes first, then one block per section:
its title as a heading, its note, and an aligned table (numbers
right-aligned, text left-aligned and cut at 50 columns with "…"; a null
cell shows `-`). A missing, unreadable or malformed file, or another
format version, is said plainly in the buffer with the path; no error is
signalled and no data is shown that is not in the file.

The buffer is drawn again when the file changes on disk: one timer
compares its modification time and size every
`ardentia-stats-check-interval` seconds (default 5; nil or 0 turns it
off). The timer starts when the view is opened and stops once its buffer
is gone. Point stays in its section, and folded sections stay folded.
Folded sections stand directly under each other; the blank line after an
open section belongs to it.

The window of the data (how many days and weeks the sections cover) is the
producer's business. If it can be told on its command line, set
`ardentia-stats-render-command` (default nil) to a list `(PROGRAM ARG...)`
or a command line string; the view appends `--days N --weeks M` and runs
it without waiting. A PROGRAM that names a path is expanded with
`expand-file-name`, a bare name is looked up with `executable-find`. The
buffer follows by itself when the command rewrites the file, and a
non-zero exit is reported in the echo area. With the option unset, `d` and
`w` only say so.

Keys: `TAB` fold or unfold the section at point, `<backtab>` (S-TAB) all
sections, `n` / `p` next / previous section, `RET` visit the task of the
row at point in another window (`org-id-goto`; rows without an ID only
say so; on a heading `RET` folds), `d` / `w` choose the days / weeks the
data cover (presets 7 14 30 90 / 4 12 26 52, any positive whole number is
accepted; both values are passed, the other one is taken from `view`, else
30 days / 12 weeks), `b` open the chart page of the file
with `browse-url-of-file`, `g` read the file again, `q` quit, `?` menu of
all keys of this buffer (`ardentia-stats-menu`; `.` opens the global
menu, as it does directly in the buffer).

#### The stats file (producer interface)

The file is UTF-8 JSON:

```json
{"version": 1,
 "generated": "2026-10-27T03:40:00+01:00",
 "html": "/abs/path/index.html",
 "note": "one line, may be null",
 "sections": [
   {"key": "model-mix", "title": "Model mix per role, last 30 days",
    "note": "optional one-liner or null",
    "columns": [{"name": "Role", "type": "text"},
                {"name": "opus", "type": "percent"}],
    "rows": [{"cells": ["orchestrator", 0.82], "id": null}]}
 ]}
```

- `version`: must be 1; any other value is reported, not rendered.
- `generated`: ISO 8601 time the file was written; shown in local time
  with its age. Anything unparsable is shown as is.
- `html`: absolute path of a page with charts for `b`, or null.
- `view`: optional, `{"days": N, "weeks": M}` (positive integers): the
  window the data were computed for; shown in the header line and used as
  the other value when `d` or `w` changes one of them. Missing or invalid
  values are ignored.
- `note`: one line shown above the sections, or null.
- `sections`: shown in this order. `key` identifies the section for
  folding and for keeping point on refresh (a missing key falls back to
  the position); `title` is the heading; `note` a line under it or null;
  `columns` name and type each column; `rows` hold one `cells` array per
  row, in column order (missing cells count as null), and an `id`: the
  org ID (`:ID:` property) of the heading the row is about, or null.

Column types and how a cell is shown:

| Type       | JSON value              | Shown as                         |
|------------|-------------------------|----------------------------------|
| `text`     | string                  | as is, cut at 50 columns with "…" |
| `int`      | integer                 | `12`                             |
| `tokens`   | integer                 | `950`, `12.3k`, `3.4M`           |
| `usd`      | number                  | `$1.23`                          |
| `percent`  | number from 0 to 1      | ` 82% ██████▌ ` (an 8-cell bar)  |
| `duration` | seconds                 | `42s`, `4m`, `1h12m`, `1d1h`     |
| `date`     | string                  | as is                            |

Every type takes null, shown as `-`. An unknown type, or a value that does
not fit its type, is shown with `format "%s"`.

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
`g` re-render, `q` quit, `z` or `C-c C-z` add a note to the task,
`:` or `C-c C-q` set the tags, `C-c C-s` schedule, `C-c C-d` deadline
(`C-u` removes either), `C-c C-t` TODO state, `,` priority, `+`/`-` (also
`S-<up>`/`S-<down>`) raise/lower it, all on the real heading of the
reviewed task, followed by a re-render, `?` menu of all keys of this buffer
(`ardentia-review-menu`; `.` opens the global menu, as it does directly in the buffer).
`z` opens org's note window (`*Org Note*`) for the reviewed task: `C-c C-c`
stores the note in the LOGBOOK of the real heading, saves the file when it
was unmodified before and re-renders the buffer, where the note shows under
"Your notes after the report"; `C-c C-k` cancels.
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
`ardentia-stale-minutes`, `ardentia-dashboard-show-later`, `ardentia-dashboard-group-reviews`, `ardentia-dashboard-group-min`, `ardentia-due-display`,
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
`ardentia-review-alert-skip-command`,
`ardentia-stats-file`, `ardentia-stats-check-interval`.

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
- `ardentia-stats-heading` (type, bold, like the group rows),
  `ardentia-stats-note` (shadow), `ardentia-stats-column` (bold): the
  agent stats view

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
- `ardentia-stats.el` agent stats view rendered from an external JSON file
- `ardentia-capture.el` new task and delegate
- `ardentia-run.el` start the runner on one task now
- `ardentia-usage.el` usage limits and runner pause in the header line
- `ardentia-modeline.el` mode-line counter
- `ardentia-alert.el` opt-in alert for new review tasks
- `ardentia-reload.el` reload all modules in place (`ardentia-reload`)
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
