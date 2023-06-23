## WHY?

I prefer my emacs to make some compromises with the surrounding system
environment, which is MacOS in my case. This configuration allows for
a fairly natural and aesthetically pleasing out-of-the-box experience
on MacOS machines.

After the release of emacs 24.1, I took the time to jettison an
enormous quantity of elisp that had accumulated in my dotfiles since
1985. Most of the custom features implemented by that code now have
analogues in base emacs or in one of the many packages available via
the various ELPA-compatible repositories. This process has made it
more practical to share the base layer of my config.

## INSTALLATION

If you would like to test out this configuration, clone this repo and
place it in your home directory as the directory `.emacs.d`.

You will, of course, also need to install emacs itself. I prefer
YAMAMOTO Mitsuharu's Mac port, also called the _railwaycat_ version,
which I install using [homebrew](http://brew.sh/).

```bash
$ brew tap railwaycat/emacsmacport && brew install emacs-mac --with-imagemagick --with-modern-icon --with-modules
```

Once this is complete, you should also install
[Hunspell](http://hunspell.github.io) and whatever dictionaries you
need for whichever languages you use. This will be used by
[Flyspell](http://www.emacswiki.org/emacs/FlySpell) to offer spell
check in any text modes you use.

I also recommend that you install [The Silver
Searcher](https://github.com/ggreer/the_silver_searcher), which is
required by [ag](https://github.com/Wilfred/ag.el):

```bash
$ brew install ag
```

My favorite free coding font is [JetBrains Mono](https://www.jetbrains.com/lp/mono/), which will be used if it is
installed. Otherwise, emacs will try to load [Fira Code](https://github.com/tonsky/FiraCode), after which it will fall
back to Apple's `Menlo`.

This configuration uses the `doom-modeline` package to provide an
aesthetically and functionally enhanced modeline. In order for it to
display properly, you will need to run: 

``` emacs-lisp
M-x all-the-icons-install-fonts
```

... to download and install the icon fonts used to decorate the
modeline.

### CUSTOMIZATION

If you would like to extend this configuration, create a
`<username>-local.el` file within your `.emacs.d/lisp` directory and
that file will be loaded after this configuration has been
initialized. You can create a separate version controlled directory
for your personal modifications and symlink to keep those extensions
in version control.

## A PRIMER

One of the goals of this configuration is to avoid the cognitive load
of switching keyboard habits between applications. Luckily, many of
the default emacs control sequences are also supported by MacOS text
editing panels and the major shells (which can be made more comformant
via one's `.inputrc` file).

The usual MacOS command key bindings are mostly supported. Command-S
saves, Command-F "finds" (searches forward) and Command-G continues
the search, cut and paste, selection, and so on all operate
normally. Some commands are modified so that they're the emacs
semantic equivalent of their MacOS counterparts. For example,
Command-w "kills" (closes) the current buffer rather than the emacs
window (which I feel should be either full-screen or split-screen with
a browser most of the time).

The emacs notation for key sequences looks like `a` (press a), `C-c`
(control + c), `M-x` ("meta ex", where "meta" is the alt/option key
when using this configuration on a Mac) or `s-a` ("super a", where
"super" is the command key). So, a sequence like `C-x C-f` means "hold
down the control key while pressing first `x` then `f`.

### BUFFERS, FRAMES AND FILES

`C-x C-f` is "find file," which allows one to find files quickly using
command completion in the mini-buffer. `s-O` opens a file, but does so
using a pattern-matching "find in project" function, similar to Vim's
`ctrl-p`.

These commands both work with
[tramp](http://www.gnu.org/software/tramp/), which is a great feature
that provides access to files on remote servers, inside docker
containers, and so on. One can open a file on a remote host via sftp
by specifying its name like this:

`hostname:/path/to/file`

`C-x C-b` is the command to switch the current frame (like a pane in
`tmux`; basically a subwindow) to another buffer by name. It remembers
recently open files, and so makes an easy way to open anything one has
been working on without hunting around in the file system.

Split the current frame in two vertically by hitting `C-x 2`,
horizontally by `C-x 3`. Close the current frame (but not the
underlying buffer/file) with `C-x 0`. Close all *but* the current
frame with `C-x 1`.

Switching between multiple visible buffers is done using the arrow
keys modified by `M-s`, which is to say that `M-s-left` will switch
focus to the frame to the left of the currently active one. I've set
up my browsers and iTerm2 to accept this same shortcut to move between
tabs and shells.

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
`sexp` (S-expression in lisp terms, but some other _unit of code_ in
other languages).

The usual MacOS bindings for begin/end of line (`s-left arrow`,`s-right
arrow`) and top/bottom of document (`s-up arrow`, `s-down arrow`),
page up and page down, and so forth, are also supported.

All of these bindings can be combined with the `shift` key to select
text.

### SEARCHING

Although the above key combinations provide for rapid navigation, one
should generally use them only for short movements, preferring to jump
around inside a file using search forward (both of `C-s` and `s-f`) or
search reverse (`C-r`). Note that you can continue a search using
`s-g`, and that it will wrap from end of the file to beginning (or
vice versa, depending on search direction).

Also, if the target is visible on screen,
[ace-jump-mode](http://www.youtube.com/watch?v=UZkpmegySnc) is
lovely. It is bound to `C-space`.

The marvelous
[visual-regexp](https://github.com/benma/visual-regexp.el) is bound to
`s-r` (and `visual-query-regexp` to `s-R`). These functions provide
regular expression query/replace with a live preview of matches and
replacements. The latter 'query' version iterates over the matches,
replacing ones where the user presses `space` and skipping the ones
where the user presses `delete`.

To find everything that matches in the current buffer, check out
`occur`, which is bound to `M-s-o`. It's worth perusing the occur's
documentation, as it's quite powerful and can be made to do
interesting things using the [_universal argument_](https://irreal.org/blog/?p=1613).

For searching across multiple files,
[ag](https://github.com/Wilfred/ag.el) provides a simple interface
that can be invoked with `M-x ag`.

### DELETING ("KILLING")

Like the arrow keys, deletion (called "killing" in emacs) is amplified
with modifier keys. `delete` does what one would expect, `M-delete`
deletes a word at a time, `C-M-delete` deletes a paragraph at a
time. These deletions go into the `kill-ring` for future `yank`ing
(pasting).

`fn-delete` deletes to the right (as in other MacOS inputs),
`fn-M-delete` deletes to the right one word at a time, and
`fn-C-M-delete` deletes the paragraph to the right.

### UNDO/REDO

As in the other programs: Command-z is undo, Shift-Command-Z is redo.

## EXPLORING EMACS

Getting used to the idea that emacs isn't a text editor in the usual
sense, but a Lisp environment where a given series of keystrokes are
assigned to invoke a particular function is important in learning how
to use and customize it.

The collection of functions available in emacs can be explored and
invoked by typing M-x and then starting to type a known or probable
function name. This will bring up the completion interface in the
mini-bar, starting with the most recently executed function. One often
does things like `M-x re<TAB><RET>` to complete and invoke
`replace-string`.

Consider using the `apropos` function when you know roughly what you
want to do, but aren't sure of the exact function that will do
it. `M-x apropos` prompts you to enter a word (or words) related to
what you'd like to do, and creates a buffer for you to browse various
functions related to your search term.

Emacs has a ubiquitous help system that allows us to find the binding
of any key, purpose of any function, and so on. Help commands start
with `C-h` (for help!), then a series of letters to indicate what kind
of help is being requested. Two of the most useful forms of help for
new users are variations on _describe_: `C-h k` (help -> describe
keybinding) brings up a prompt that will listen for a series of
keystrokes, then report what function is called by that sequence;
while `C-h f` (help -> describe function) will provide a similar
service for functions using a completion interface. Function
descriptions will also list the shortcuts keys that are bound to that
function.

If you have trouble remembering a particular key combination, the
`which-key` package tries to help. If it detects a moment of
hesitation after entering an incomplete key combination, it will show
you a list of possible completions. For instance, in the previous
paragraph we discussed two possible completions for `C-h`: `C-h k` to
describe a keybinding, and `C-h f` to describe a function. You can
discover other help functions by keying just `C-h` and reviewing the
many possible completions.

## SOURCE CONTROL

[Magit](https://github.com/magit/magit) is a nice integrated git
for emacs. There are others like it, but this one is my favorite.

## INTERACTIVE PROGRAMMING LANGUAGES

SLIME, nREPL, run-ruby, run-python, and so on. There's a great deal of
power when interacting with external interpretors in emacs, but it's
more than I have time to write about just now. TK.

In emacs lisp, scheme and clojure modes, `eval-defun` (which evaluates
the top level of the current lisp form) and `eval-last-sexp` are bound
to `s-enter` and `s-shift-enter` respectively. In most cases the
evaluated code will flash momentarily to indicate the scope in which
the evaluation occurred. Also, short documentation for the current
function should be visible in the mini-buffer and `C-c C-d d` should
pop up further docs on the symbol at point.

You can also jump to the definition of the current function or macro
using `M-.` and return from the definition with `M-,`.

## RESOURCES

Watch this video on
[`Expand region`](https://github.com/emacsmirror/expand-region). This is
a feature every code editor should have. In this configuration, it's
bound to `s-1` and `contract-region` is bound to `s-2`, which allows
one to press `s-1` repeatedly without concern for overshooting the
intended selection because it's easy to contract if we go to far.

If one intends to hack clojure, Common Lisp, scheme or elisp, it would
be wise to get to know
[SmartParens](https://github.com/Fuco1/smartparens/wiki/Paredit-and-smartparens),
which has been added as a replacement for _paredit_.
