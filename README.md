# happy-history

`(reverse-i-search)` _(Ctrl+R)_ replacement.

## How is this different to XXX?
There are many tools out there to enhance history and searchability. To name a few I found and were inspired by _(in alphabetical order)_:

- [ellie/atuin](https://github.com/ellie/atuin)
- [dvorka/hstr](https://github.com/dvorka/hstr)
- [cantino/mcfly](https://github.com/cantino/mcfly)

But all those, and frankly, the default `history` lack _(in my view)_ is good ergonomics to find and use part of a previously executed command.

## Why does this even exist?
Some times, I want to do something like:

> I want to find what program is using up port 4000 and 4001.

And my typical workflow goes like this:
1. Search "linux netstat program port".
2. Open the _same_ [StackOverflow](https://unix.stackexchange.com/questions/106561/finding-the-pid-of-the-process-using-a-specific-port) question that has the snippet: `netstat -nlp | grep 9000`
3. Copy that command and execute.
4. Up arrow (↑) to re-type the previous command.
5. `Ctrl + w` to delete the last `9000`. 
6. Type in `4000`
7. Realize I'm not logged as super user
8. Up arrow (↑) to re-type the previous command.
9. `Home` to go the the start of the line.
10. Type in `sudo`.
11. `Enter`. **Success** I found the program on port `4000`.
12. Up arrow (↑) to re-type the previous command.
13. `Ctrl + w` to delete the last `4000`.
14. Type in `4001`.
15. `Enter`. **Success** I found the program on port `4001`.

Many might know that step 9 and 10 could be replace by typing: `sudo !!`, as `!!` get expanded to the to the previous command executed.

Some might even know that steps 12, 13 and 14 can be done by doing `^4000^4001^`.

From the manual of `history`:
```
^string1^string2^
    Quick  substitution.   Repeat  the  last  command,  replacing  string1 with string2.  Equivalent to
    ``!!:s/string1/string2/'' (see Modifiers below).)
```

But, most notable for me is that steps 4, 5 and 6 can be replaces with: `!:0- 4000`. This can be read as: “Re-run the last command (`!`) but take the arguments from the start (`0`) to the last word (`-`) and then _concatenate_ `4000`”.

Long gone are the days where we have terminals and no arrow keys. This makes no sense why there is no TUI-like utility. Well… there _was_ no TUI-like utility until now!!

## Usage
### Installation
The recommended way of using `happy-history` is to do some bash/zsh _magic_ to bind the execution. But it is not necessary.

### Navigation
Once you are _in_ `happy-history`, you can do exactly the same as you were doing in `reverse-i-search`. To reiterate, this is a drop-in replacement of that functionality. So `sudo !!`, `^4000^4001^`, et al. all keep working

### Keybinds
One in `happy-history` you can type as you did in `reverse-i-search`.
You can scroll though the results with `Ctrl + R` or the vertical arrow keys (↑/↓).
Pressing horizontal arrows keys (→/←) focuses on the command. There you can move about and edit in place.
To go back to searching, press `Ctrl + R` or the vertical arrow keys (↑/↓) again.

### All key binds
| Default Key bind |  Action |
| --- | --- |
| Ctrl + R | Search the previous occurrence of the searched term |
| Esc | Exit |
| Enter |Execute the command |
| ↑/↓ | Scroll though occurrences of the searched term. If no term was searched, it scrolls though previous commands*. |
| →/←/Home/End | Switch focus to the command editor, and move the cursor around |
| Ctrl + E | Open an editor with the current command. The resulting command will be executed. _sort of like [`fc`](http://www.gnu.org/software/bash/manual/bash.html#Bash-History-Builtins)_ |

## To be defined
- How to install in bash (`bind`? vi mode / emacs mode)
- How to install in zsh (zsh plugin?)
- Should we deal with pipes as a different event?
- Can `hh` _type_ the command without executing it?

## Development
Check [Architecture.md](./Architecture.md) for a more in-depth explanation of the source code and the development.