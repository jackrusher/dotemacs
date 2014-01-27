## WHY?

After the release of emacs 24.1, I took the time to jettison an
enormous quantity of elisp that had accumulated in my dotfiles since
1985. Most of the custom features implemented by that code now have
analogues in the base emacs or in one of the many excellent packages
available via the ELPA-compatible repositories.

## INSTALLATION

If you would like to test out this configuration, clone this repo and
place it in your home directory as the directory `.emacs.d`.

I install emacs on Mac OS X using this
[homebrew](http://brew.sh/) recipe:

$ brew install emacs --HEAD --use-git-head --cocoa --srgb

Note that this configuration automatically installs a somewhat large
number of packages via ELPA/MELPA/Marmalade the first time emacs is
run with it. This takes time, and there may be transient errors that
require one to quit and restart a couple of times to get it done.

### CUSTOMIZATION

If you would like to extend this configuration, create a
`<username>-local.el` file within your `.emacs.d` directory and that
file will be loaded after this configuration has been initialized.

## A PRIMER

One of the goals of this configuration is to avoid the cognitive load
of switching keyboard habits between applications. Luckily, many of
the default emacs control sequences are also supported by OSX text
editing panels and the major shells (and can be made more comformant
via one's inputrc).

The usual OSX command key bindings are mostly supported. Command-S
saves, Command-F "finds" (searches forward), cut and paste, selection,
and so on all operate normally. Some commands are modified so that
they're the emacs semantic equivalent of their OS X counterparts. For
example, Command-w "kills" (closes) the current buffer rather the
window (which should be full-screen most of the time).

The emacs notation for key sequences looks like `a` (press a), `C-c`
(control + c), `M-x` ("meta ex", where "meta" is the alt/option key in
this configuration) or `s-a` ("super a", where "super" is the command
key). So, a sequence like `C-x C-f` means "hold down the control key
while pressing first 'x' then 'f'.

### BUFFERS, FRAMES AND FILES

Ctrl-x Ctrl-f is "find file," which allows one to find files quickly
using command completion in the mini-buffer. Command-O opens a file,
but does so using a pattern-matching "find in project" function,
similar to Vim's `ctrl-p`.

These commands both work with
[tramp](http://www.gnu.org/software/tramp/), which is a great, great
feature. One can open a file on a remote host via sftp by specifying
its name like this:

`hostname:/path/to/file`

Ctrl-x Ctrl-b is the command to switch the current frame (like a pane
in tmux, basically a subwindow) to a buffer by name with command
completion. It remembers recently open files, so it makes an easy way
to open anything one has been working on without hunting around in the
file system.

Split the current frame in two vertically by hitting Ctrl-x 2,
horizontally by Ctrl-x 3. Close the current frame (but not the
underlying buffer/file) with Ctrl-x 0. Close all *but* the current
frame with Ctrl-x 1.

Switching between multiple visible buffers is done using the arrow
case modified by `M-s`. I've set up my browsers and iTerm2 to accept
this same shortcut to move between tabs and shells.

### NAVIGATION

The emacs-native navigation keys (`C-b`, `C-f`, `C-p`, `C-n` for
backward character, forward character, previous line, next line) are
easy and efficient once internalized, but they predate and differ from
every other system one is likely to use. This configuration prefers
the arrow keys, but maintains the (IMO) most important aspect of the
emacs philosophy of movement: the base movement keys can be amplified
with modifier keys. `M-left` will move one word to the left, `M-up
arrow` will move up one paragraph, and so on. Of particular interest
for programming modes, `C-M-left arrow` will navigate left by one
`sexp` (S-expression in lisp terms, but some other "unit of code" in
other languages).

The usual OS X bindings for begin/end of line (`s-left arrow`,`s-right
arrow`) and top/bottom of document (`s-up arrow`, `s-down arrow`), page up and page down, and so forth, are also supported.

### SEARCHING

Although the above key combinations provide for rapid navigation, one
should generally use them only for short movements, preferring to jump
around inside a file using search forward (both of `C-s` and `s-f`) or
search reverse (`C-r`). Also, if the target is visible on screen,
[ace-jump-mode](http://www.youtube.com/watch?v=UZkpmegySnc) is a
lovely. It is bound to `C-space`.

The marvelous
[visual-regexp](https://github.com/benma/visual-regexp.el) is bound to
`s-r` (and `visual-query-regexp` to `s-R`). These functions provide
regular expression query/replace with a live preview of matches and
replacements. The latter 'query' version iterates over the matches,
replacing ones where the user presses `space` and skipping the ones
where the user presses `delete`.

### DELETING ("KILLING")

Like the arrow keys, deletion (called "killing" in emacs) is amplified
with modifier keys. `delete` does what one would expect, `M-delete`
deletes a word at a time, `C-M-delete` deletes a paragraph at a
time. These deletions go into the `kill-ring` for future `yank`ing
(pasting).

`fn-delete` deletes to the right (as in other OS X inputs),
`fn-M-delete` deletes to the right one word at a time, and
`fn-C-M-delete` deletes the paragraph to the right.

### UNDO/REDO

Command-z is undo, Shift-Command-Z is redo. This functionality is
provided by a package called
[undo-tree](http://www.emacswiki.org/emacs/UndoTree), which is similar
to the vi package of the same name. It allows one to see recent
changes as a decision tree and partially back out changes by choosing
branches (like a mini RCS in the editor). The undo visualizer is bound
to Control-Command-z.

## EXPLORING EMACS

Getting used to the idea that emacs isn't a text editor in the usual
sense, but a Lisp environment where a given series of keystrokes are
assigned to invoke a particular function is important in learning how
to use and customize it.

The collection of functions available in emacs can be explored and
invoked by typing M-x and then starting to type a known or probable
function name. This will bring up the completion interface in the
mini-bar, starting with the most recently executed function. One often
does things like:

*M-x re<TAB><RET>* to complete and invoke "replace-string"

Emacs has a ubiquitous help system that allows one to find out the
binding of any key, purpose of any function, and so on. Help commands
start with ctrl-h (for help!), then a series of letters to indicate
what kind of help is being requested. Two of the most useful forms of
help for new users are variations on "describe": *ctrl-h k* (help ->
describe keybinding) brings up a prompt that will listen for a series
of keystrokes, then report what function is called by that sequence;
*ctrl-h f* (help -> describe function) will provide a similar service
for functions using a completion interface. Function descriptions will
also list the shortcuts keys that are bound to that function.

## SOURCE CONTROL

[Magit](https://github.com/magit/magit) is a nice integrated git
for emacs. There are others like it, but this one is my favorite.

## DYNAMIC LANGUAGES

SLIME, nREPL, run-ruby, run-python, and so on. There's a great deal of
power when interacting with external interpretors in emacs, but it's
more than I have time to write about just now. TK.

In emacs lisp, scheme and clojure modes, `eval-defun` (which evaluates
the top level of the current lisp form) and `eval-last-sexp` are bound
to `s-enter` and `s-shift-enter` respectively. In most cases the
evaluated code will flash momentarily to indicate the scope in which
the evaluation occurred. Also, short documentation for the current
function should be visible in the mini-buffer and `C-c d` should pop
up further docs on the symbol at point.

## RESOURCES

Watch this video on
[Expand region](https://github.com/emacsmirror/expand-region). This is
a feature every code editor should have. In this configuration, it's
bound to `s-1` and "contract-region" is bound to `s-2`.

If one intends to hack clojure, Common Lisp, scheme or elisp, it would
be wise to get to know
[SmartParens](https://github.com/Fuco1/smartparens/wiki/Paredit-and-smartparens),
which has been added as a replacement for _paredit_.

Those who build the web should look into
[skewer-mode](https://github.com/skeeto/skewer-mode) for live browser
mind control.
