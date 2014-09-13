# battlelogs
- - -
A minimal personal journaling system, for the command-line, I wrote for myself.

## Installation
To install, clone this repository to your machine and run:
```
cabal install
```

## Usage
```
Usage:
      blg init
      blg commit [-m=<message>]
      blg show
      blg [-h]
Options:
      -h  Show this help message
```

To add entries to the battle log do:
```
blg commit -m "Hello World"
```

This will create a `.battlelogs.md` file in your `$HOME` directory and add an
entry to it. Typing `blg commit` without the `-m` flag will cause `blg` to open
your default text editor (`$EDITOR`) and capture its output, adding it to the
logs file.

The `blg init` command is meant to create "local" battle logs. Running it will
create an empty `.battlelogs.md` file in the current directory, which will
automatically become `blg`'s output file.

`blg show` (as of right now) will simply open the `.battlelogs.md` file in use
in the system's `$PAGER`. Pretty colors and a better logging display (and
format) are a _todo_.

## License
This code is licensed under the GPLv2 License. See [LICENSE](/LICENSE) for more
information.
