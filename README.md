# comprex - Analyze and Cleanup Directory of Compressed Files

![GitHub release (latest by date)](https://img.shields.io/github/v/release/telostat/comprex)
![GitHub contributors](https://img.shields.io/github/contributors/telostat/comprex)
![GitHub](https://img.shields.io/github/license/telostat/comprex)

> **TODO:** Provide a complete README.

## Usage

General program usage:

```
$ comprex --help
comprex - Analyze and Cleanup Directory of Compressed Files

Usage: comprex [--version] COMMAND
  comprex

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  analyze                  Analyze a given directory and report
  cleanup                  Remove uncompressed files that are successfully
                           compressed
```

`analyze` command usage:

```
$ comprex analyze --help
Usage: comprex analyze (-d|--dir DIR)
  Analyze a given directory and report

Available options:
  -d,--dir DIR             Directory to analyze
  -h,--help                Show this help text
```

Example:

```
$ cp example/* /tmp &&comprex analyze -d /tmp | xsv table
FileA       HashA                                     FileB          HashB                                     Match
/tmp/0.txt  09d2af8dd22201dd8d48e5dcfcaed281ff9422c7  /tmp/0.txt.gz  3f786850e387550fdab836ed7e6dc881de23001b  False
/tmp/c.txt  2b66fd261ee5c6cfc8de7fa466bab600bcfe4f69  /tmp/c.txt.gz  2b66fd261ee5c6cfc8de7fa466bab600bcfe4f69  True
/tmp/a.txt  3f786850e387550fdab836ed7e6dc881de23001b  /tmp/a.txt.gz  3f786850e387550fdab836ed7e6dc881de23001b  True
/tmp/b.txt  89e6c98d92887913cadf06b2adb97f26cde4849b  /tmp/b.txt.gz  89e6c98d92887913cadf06b2adb97f26cde4849b  True
```

`cleanup` command is not yet implement.

## Installation

```
stack install [--flag comprex:static]
```

... or download the precompiled binary under releases.


## License

Copyright Telostat Pte Ltd (c) 2021.

This work is licensed under MIT. See [LICENSE](./LICENSE).
