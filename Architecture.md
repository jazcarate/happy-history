# Architecture

## Tools
`happy-history` is written in Haskell, using the [`stack`](https://docs.haskellstack.org/) tool-chain.
To build from sources, or develop I'm using `ghc` 8.10.4, `cabal`: 3.2 and `stack`: 2.7.1; though I reckon later version should work just as well.
The source code was scaffolding with [`RIO`](https://hackage.haskell.org/package/rio).

### Development
One the pre-requisite tools are installed, you can run `hh` from sources with:
```bash
stack run
```

Tests can be run with
```bash
stack test
```

## Bash bind command explained
WIP: This only works in bash, with emacs mode. I'd expect this to be the norm.

```
bind "'\C-y': '\C-a history > /tmp/.hh_history; hh -- \C-e; cmd=\$(cat /tmp/.hh_last_command); history -s \$cmd; echo \${PS1@P} \$cmd; \$cmd\C-m'"
                ^   ^^       ^                   ^            ^      ^   ^     ^                       ^                 ^              ^     ^-- Execute the whole line
                |   ||       |                   |            |      |   |     |                       |                 |              +-- Actually run the command
                |   ||       |                   |            |      |   |     |                       |                 +-- Re echo the prompt and the command (to fake the command being written)
                |   ||       |                   |            |      |   |     |                       +-- Store the new command in the history
                |   ||       |                   |            |      |   |     +-- `hh` stores the command in this temp file
                |   ||       |                   |            |      |   +-- Run `cat`
                |   ||       |                   |            |      +-- Store the command ☝️
                |   ||       |                   |            +-- Go to the end of the line
                |   ||       |                   +-- Run `hh`, with arguments being what you just typed**
                |   ||       +-- Store the history in a tmp file so that `hh` can access it.
                |   |+-- Get current history
                |   +-- The space is important, to ignore this added to the history itself
                +--- Go to the beginning og the line (emacs style)
```

*: as `history` not a proper program, there is no other way of getting the most up to date history.
**: Replace with `stack run` for development use

bind "'\C-y': '\C-a history > /tmp/.hh_history; stack run -- -- \C-e; cmd=\$(cat /tmp/.hh_last_command); history -s \$cmd; echo \${PS1@P} \$cmd; \$cmd\C-m'"