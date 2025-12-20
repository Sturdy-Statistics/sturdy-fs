# sturdy.fs

A small collection of file system utilities for Clojure.

This library provides a thin, opinionated wrapper around [`babashka.fs`](https://github.com/babashka/fs) for common tasks such as reading and writing files, handling byte arrays, and working with paths.

[![Clojars Project](https://img.shields.io/clojars/v/com.sturdystats/sturdy-fs.svg)](https://clojars.org/com.sturdystats/sturdy-fs)

```clj
com.sturdystats/sturdy-fs {:mvn/version "VERSION"}
```

## Features

- Read files as bytes, strings, EDN, or lines
- Write bytes, strings, or EDN with automatic parent directory creation
- Optional atomic file writes
- Simple helpers for path manipulation
- POSIX permission helpers (`chmod-600!`, `chmod-400!`)

## Design

- Minimal API
- Explicit error checking (via `taoensso.truss`)
- No global state
- Intended as a small shared utility library, not a full filesystem abstraction

## Non-goals

- Streaming or lazy IO
- File watching
- Platform-specific abstractions beyond what `babashka.fs` provides

<!-- Local Variables: -->
<!-- fill-column: 1000000 -->
<!-- End: -->
