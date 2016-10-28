## Basics

Installation

```sh
brew install ocaml opam rlwrap
```

Shortcuts for "rlwrapped" ocaml
```
$ cat `which ocaml-rl`
#!/bin/bash
exec rlwrap ocaml "$@"
```

Quit top-level rlwrap
```ocaml
#quit;; (* with the # *)
```
