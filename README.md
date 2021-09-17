# cal-clone

This aims to be a clone of the `cal` command found on most unix systems, with a
couple differences. I found, in order to display Monday as the first day of the
week, one had to use the command in `ncal` mode, which I didn't like. In this
version, Monday is the day the week starts on.

Oh, and it's written in Haskell. Basically only for the reason that it's a
pretty simple task to solve in Haskell. Sue me for using my pet language.

## Installation

I haven't put this anywhere like hackage, so I'm afraid if you want to use this,
you'll have to build from source. You'll need stack for this:

```sh
$ stack build
$ stack install mal
```

The executable's named mal, for Monday cAL. By default, this will install into
your `~/.local/bin` folder, so make sure that's on your path.

I have literally never tested this on Windows, so I have no idea if this will
work. The code should compile just fine, so long as you have stack (although it
might be put into a different directory). However, the code uses ANSI escape
sequences to color the text, so that's 50/50 as to whether your Windows'
terminal craps out over that. If it does, it's probably a sign to try Windows
Terminal?

## Things still to do

Roughly in order of priority:

- [ ] Specifying a month other than the current one.
- [ ] Showing more than one month (like `cal`'s `-3` option).
- [x] Refactor to use a pretty printer
- [ ] Tests?

Feel free to raise an issue if you think something should be added to this list.
