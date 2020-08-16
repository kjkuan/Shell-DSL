NAME
====

**Shell::DSL** - Run and pipe commands from one to another like you would in Bash.

DESCRIPTION
===========

`Shell::DSL` is a module that provides a shell-like experience for running external commands and redirecting their I/O's. A command's standard output can be captured, and commands can be connected to form a pipeline easily.

**NOTE:** This module is not thread-safe.

**Caveat:** Although an implementation detail, this module currently depends on Bash to connect the pipes and to set up user specified I/O redirections for sub processes.

SYNOPSIS
========



```perl6
use Shell::DSL;
my @words;
shell :!pipefail, {
    .curl<-fsSL https://en.wikipedia.org/wiki/Special:Random>
      |> .xmllint(«--html --xpath '//*[name()!="script"]/text()' -», (:w2</dev/null>))
      |> .tr<-cs A-Za-z  \n>
      |> .tr<A-Z a-z>
      |> .sort
      |> .uniq<-c>
      |> .sort<-rn>
      |> .head<-30>
      |> pb({
          for .lines {
              my $match = $_ ~~ /\s* \d+ ' ' (\w+)$/;
              next if !$match;
              @words.push: $match[0];
              .put;
          }
      })
      |> .nl;
}
put();
my $i = (^@words).pick;
say "The word, '@words[$i]', is No. {$i+1} in the list.";
```

See *examples/* for more usage examples.

AUTHOR
======

Jack Kuan <kjkuan@gmail.com>

COPYRIGHT AND LICENSE Copyright 2020 Jack Kuan
==============================================

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

class Shell::DSL::Proc::Async::SyncWriter
-----------------------------------------

Wraps a Proc::Async instance so that it can be used as an output file handle.

class Shell::DSL::Command
-------------------------

A `Command:D` captures all the info (path, arguments, environment variables..., etc.) needed to execute an external command. A command's PID and exit status are also stored in the `Command:D` at the end of its execution. A `Command:D` is usually created and executed once, usually on the spot, or as part of a command `Pipeline:D`. There are a few ways to execute a command: 1. By sinking a `Command:D`. 2. By calling its `run` method directly. 3. By calling its `capture` method directly. 4. By evaluating a `Command:D` in a boolean context. 5. By evaluating a `Command:D` in a string context. 6. By evaluating a `Command:D` in a numeric context.

### method rc

```perl6
method rc() returns Int:D
```

The exit status of the command's last execution. Greater than 128 means it's been killed by a signal, whose number you can get by subtracting 128 from the returned exit code.

### method run

```perl6
method run() returns Shell::DSL::Command:D
```

Run the command and wait for it to exit.

### method capture

```perl6
method capture(
    Bool:D :$chomp = Bool::True,
    Bool:D :$check = Bool::True,
    Str:D :$encoding = "utf8"
) returns Str:D
```

Run the command and return captured STDOUT (text).

### method CALL-ME

```perl6
method CALL-ME(
    |c
) returns Mu
```

Same as `capture`.

### method Str

```perl6
method Str() returns Str:D
```

Same as `capture`, except you can't pass arguments to it.

### method Bool

```perl6
method Bool() returns Bool
```

Run the command if not already run, wait for it to exit, and return True if its exit status is successful; return False otherwise.

### method Numeric

```perl6
method Numeric() returns Int:D
```

Run the command if not already run, wait for it to exit, and return its exit status.

### method sink

```perl6
method sink() returns Nil
```

Run the command if not already run. Throws a `CommandError` exception if the command failed.

class Shell::DSL::Pipeline
--------------------------

A `Pipeline` is a `Command` made up of a sequence of `Command`'s connected via pipes (see `man 2 pipe`).

### method new

```perl6
method new(
    *@parts where { ... }
) returns Mu
```

You should create a `Pipeline:D` via the `|>` operator instead of calling `.new`.

### method run

```perl6
method run(
    Bool :$capture,
    Bool :$merge
) returns Shell::DSL::Command:D
```

Run the pipeline and wait for it to exit.

### method capture

```perl6
method capture(
    Bool:D :$chomp = Bool::True,
    Bool:D :$check = Bool::True,
    Str:D :$encoding = "utf8"
) returns Str:D
```

Run the pipeline, wait for it to exit and capture its STDOUT, which is that of the last command in the pipeline.

### method Bool

```perl6
method Bool() returns Bool
```

Run the pipeline if not already run, wait for it to exit, and return `True` if the execution is successful; return `False` otherwise. When the `:pipefail` option of `&shell` is `True` (the default), a pipeline execution is successful only if ALL commands in the pipeline exit with a `0` status code. When `:pipefail` is `False`, a pipeline execution is successful as long as the exit status of the last command in the pipeline is 0.

### method Numeric

```perl6
method Numeric() returns Int:D
```

Run the pipeline if not already run, wait for it to exit, and return its exit status, which is that of the last command in the pipeline.

class pb { … }
--------------

A block or callable that can be used in a `Pipeline` as a filter.

### method new

```perl6
method new(
    &block,
    Bool :$bin,
    Str :$enc,
    Bool:D :$chomp = Bool::True
) returns Mu
```

You should create a `PipeBlock:D` by calling the `&pb` sub with a `Block`.

### method cd

```perl6
method cd(
    $path = Code.new
) returns IO::Path:D
```

Change the execution directory for commands run via this Shell instance.

