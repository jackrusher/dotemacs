## WHY?

After the release of emacs 24.1, I took the time to jettison an
enormous quantity of elisp that had accumulated in my dotfiles since
1985. Most of the custom features implemented by that code now have
analogues in the base emacs or in one of the many excellent packages
available via the ELPA-compatible repositories.

## INSTALLATION

If you would like to test out this configuration, clone this repo and
place it in your home directory as the directory `.emacs.d`.

I install emacs using this
[homebrew](http://mxcl.github.com/homebrew/) recipe:

$ brew install emacs --HEAD --use-git-head --cocoa --srgb

Note that this configuration automatically installs a somewhat large
number of packages via ELPA/MELPA/Marmalade the first time emacs is
run with it. This takes time, and there may be transient errors that
require one to quit and restart a couple of times to get it done.

## NAVIGATION

One of the goals of this configuration is to avoid the cognitive load
of switching keyboard navigation habits between applications. Luckily,
most of the default emacs control codes are also supported by OSX text
editing panels and bash/zsh (via inputrc).

This table contains the most commonly used bindings between the
different modes, most of which are normalized. A pattern to keep in
mind with regard to "arrow navigation" is that the option key acts as
a magnifier (move by words and paragraphs instead of characters and
lines), and control+option magnifies even further (move by "units of
code", a sexp in Lisp, other syntactic constructs in other languages).

Note: one generally should not move very far using any form of arrow
navigation. The preferred method of jumping around inside a file in
emacs is to use search forward (ctrl-s/command-f) or search reverse
(ctrl-r) to jump. (Or, if the target is visible on screen,
[ace-jump-mode](http://www.youtube.com/watch?v=UZkpmegySnc), which is
bound to C-SPC).

| Key           | emacs                     | shell/inputrc      | OSX
|:--------------|:-------------------------:|:------------------:|------:|
| Left/Right             | Left/right character    | *         | *
| Up/Down                | Up/Down character       | *         | *
| Option-Left/Right      | Left/right word         | *         | *
| Option-Up/Down         | Up/Down paragraph       | *         | *
| Ctrl-Option-Left/Right | Left/right unit of code | *         | X
| Command-Left/Right     | Begin/End line          | X         | *
| Command-Up/Down        | Begin/End document      | X         | *
| fn-Left/Right          | Begin/End document      | X         | *
| fn-Up/Down             | Up/Down Page            | X         | *
| command-option-l/r/u/d | Switch visible buffer   | *         | X
| Delete            | Delete char left          | *              | *
| Option-Delete          | Delete word left          | *              | *
| Ctrl-Option-Delete     | Delete paragraph left          | *              | *
| fn+Delete         | Delete word left          | *              | *
| fn+option+Delete  | Delete word right         | *              | *
| Ctrl-a        | Beginning of line         | *                  | *
| Ctrl-b        | Back one character        | *                  | *
| Ctrl-c        | Command (start sequence)  | Cancel current job | X
| Ctrl-d        | Kill character forward    | *                  | *
| Ctrl-e        | End of line               | *                  | *
| Ctrl-f        | Forward one character     | *                  | *
| Ctrl-g        | Abort current action      | *                  | X
| Ctrl-h        | Help (start sequence)     | Delete             | Delete
| Ctrl-i (TAB)  | Indent line               | Complete           | X
| Ctrl-j        | Newline + indent          | Enter              | X
| Ctrl-k        | Kill line forward         | *                  | *
| Ctrl-l        | Center on current line    | Clear, show line   | *
| Ctrl-m        | Enter                     | Enter              | X
| Ctrl-n        | Next line                 | *                  | *
| Ctrl-o        | Insert line after cursor  | X                  | *
| Ctrl-p        | Previous line             | *                  | *
| Ctrl-q        | Literal insert next char  | Continue output    | *
| Ctrl-r        | Reverse search            | *                  | X
| Ctrl-s        | Search forward            | Stop output        | X
| Ctrl-t        | Transpose characters      | *                  | *
| Ctrl-u        | Universal argument        | *                  | X
| Ctrl-v        | Page down                 | X                  | *
| Ctrl-w        | Kill word backward        | *                  | X
| Ctrl-x        | Execute (start seq)       | *                  | X
| Ctrl-y        | Yank previously killed    | *                  | *
| Ctrl-z        | Suspend process           | *                  | X

The usual OSX command key bindings are mostly supported. Command-S
saves, Command-F "finds" (searches forward), cut and paste, selection,
and so on all operate normally.

Command-z is undo, Shift-Command-Z is redo. This functionality is
provided by a package called
[undo-tree](http://www.emacswiki.org/emacs/UndoTree), which is similar
to the vi package of the same name. It allows one to see recent
changes as a decision tree and partially back out changes by choosing
branches (like a mini RCS in the editor). The undo visualizer is bound
to Control-Command-z.

## BUFFERS, FRAMES AND FILES

Ctrl-x Ctrl-f is "find file," which allows one to find files quickly
using command completion in the mini-buffer.

These commands both work with
[tramp](http://www.gnu.org/software/tramp/), which is a great, great
feature. One can open a file on a remote host via sftp by specifying
its name like this:

hostname:/path/to/file

Ctrl-x Ctrl-b is the command to switch the current frame (like a pane
in tmux, basically a subwindow) to a buffer by name with command
completion. It remembers recently open files, so it makes an easy way
to open anything one has been working on without hunting around in the
file system.

Split the current frame in two vertically by hitting Ctrl-x 2,
horizontally by Ctrl-x 3. Close the current frame (but not the
underlying buffer/file) with Ctrl-x 0. Close all *but* the current
frame with Ctrl-x 1.

I also add a keybinding to Safari that normalize its tab-switching
command to Chrome's default of command+option left/right, which is
also supported by iTerm2. This emacs configuration borrows that
binding with a twist: one can switch to any visible buffer by using
command+option left/right/up/down.

## EXPLORING EMACS

Getting used to the idea that emacs isn't a text editor in the usual
sense, but a Lisp environment where a given series of keystrokes are
assigned to invoke a particular function is important in learning how
to use and customize it.

The collection of functions available in emacs can be explored and
invoked by typing M-x ("meta ex", bound to option-x in this
configuration) and then starting to type a known or probable function
name. This will bring up the completion interface in the mini-bar,
starting with the most recently executed function. One often does
things like:

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

[Magit](http://philjackson.github.com/magit/) is a nice integrated git
for emacs. There are others like it, but this one is my favorite.

## DYNAMIC LANGUAGES

SLIME, nREPL, run-ruby, run-python, and so on. There's a great deal of
power when interacting with external interpretors in emacs, but it's
more than I have time to write about just now. TK.

In emacs lisp, scheme and clojure modes, `eval-defun` (which evaluates
the top level of the current form) and `eval-last-sexp` are bound to
command-enter and command-shift-enter respectively.

## RESOURCES

Watch this video on
[Expand region](https://github.com/emacsmirror/expand-region). This is
a feature every code editor should have. In this configuration, it's
bound to command-1 and "contract-region" is bound to command-2.

If one intends to hack clojure, Common Lisp, scheme or elisp, it would
be wise to have a look at the
[paredit cheat sheet](http://www.emacswiki.org/pics/static/PareditCheatsheet.png).
Do not fight paredit. Paredit is your friend.

Those who build the web should look into
[skewer-mode](https://github.com/skeeto/skewer-mode) for live browser
mind control.
