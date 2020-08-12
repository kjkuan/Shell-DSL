NAME Shell::DSL - Run commands and pipe from one to another like you would in a command shell.
==============================================================================================

SYNOPSIS
========

```perl6
use Shell::DSL;

# The &shell built-in can now be passed a Callable instance (e.g., a block),
# which will be passed a CommandShell instance that can be used to invoke
# external commands.
#
# Any method name caught by CommandShell.FALLBACK creates a Command instance
# that captures the command to run, its arguments, as well as the
# environment variables (default to %*ENV at the command creation time).
#
shell {
    # A command instance, when sunk, runs the command. Running a command
    # returns the command instance too. The command instance tracks the pid
    # exit status of the command run. 
    .pwd;

    # Command arguments can be passed to the command as positional arguments.
    .ls: '-la'; 

    # Any named arguments will be set as environment variables for the command.
    .bash: <-c echo "$GREETING">, :GREETING<Hello>;

    # A sunk command instance throws an exception if it exited with a non-zero status.
    .bash: <exit 123>;    # exception

    # Evaluating a command in a boolean context runs it if it hasn't been run,
    # and returns True if it exited successfully or False, otherwise.
    say "Failed with exit code = {.rc}" unless .bash(<exit 123>);

    # You can also run a command explicitly via its 'run' method.
    my $cmd = .echo('Hello').run;
    say $cmd.rc;  # 0 (exit status)
    say +$cmd;    # same thing

    # Evaluating a command in a numeric context runs it if it hasn't been run,
    # and returns the exit code of the run.
    say 'success' if .echo('hello') == 0;

    # A command's STDOUT can be captured like this:
    my $iam = .whoami.capture;

    # Alternatively, you can also capture by calling the command.
    # Hint: The '()' looks like it's capturing something.
    $iam = .whoami.();

    # Evaluating a command in a string context runs and returns the captured
    # standard output.
    say "It's me!" if .whoami.tc eq 'Raku';

    # To capture the STDERR, you'll need to redirect it to STDOUT like this:
    .docker(<logs some_container>).(:stderr);  

    # Capturing, by default, chomps the trailing newline. Therefore:
    .echo('hello') eq 'hello';

    # To keep the trailing newline:
    .echo('hello', :!chomp) eq "hello\n";

    # Moreover, capturing, by default, checks the command exit status and throws
    # an exception if the command has failed.
    say .bash(<-c 'echo hello; exit 1'>).();  # You will get an exception.

    # If you only want the captured output and don't care about exit status
    # then you can disable the check like this:
    my $greeting-cmd = .bash(<-c 'echo hello; exit 1'>, :!check);
    my $greeting = $greeting-cmd.();
    say $greeting;         # hello
    say $greeting-cmd.rc;  # 1
}

# It's also possible to pipe from one command to another.
shell {
    # 'cd' is a "built-in" method of the Shell class that changes
    # the invocation directory of external commands.
    .cd: $*HOME;

    # A pipe is represented with the infix operator: '|>'
    .ls('-la') |> .grep('.raku$') |> wc('-l');

    # You can pipe an external command to or from a block as well.
    .find('.')
      |> pb({ .put for .lines.grep(/raku/) })
      |> .sort('-r');

    # Notice that blocks within a pipeline needs to be wrapped with &pb, which
    # creates a PipeBlock instance from the block, so it can be used with '|>'.
    #
    # In the first block above, .lines reads from $*IN, which in turn reads
    # from the output of 'find'. The .put writes to $*OUT, which in turn writes
    # to the pipe from which 'sort' reads from.

    # The output of a pipeline is the standard output of the last command in
    # the pipeline, and you can capture it too:
    my $result = (
        .find('.')
          |> pb({ .put for .lines.grep(/raku/) })
          |> .sort('-r')
       ).capture;

    # Alternatively, you can pipe it to another block and then read the lines
    # into an array like this:
    my @result;
    .find('.')
      |> pb({ .put for .lines.grep(/raku/) })
      |> .sort('-r')
      |> pb({ @result = .lines });
}

shell {
    my $pkg = <https://rakudo.org/dl/rakudo/rakudo-moar-2020.07-01-linux-x86_64.tar.gz>;
    my $file = $pkg.IO.basename;
    .curl($pkg) > $file;
    .tar: « -xzvf "$file" »;
}
```

DESCRIPTION
===========

`Shell::DSL` is a module that provides a shell-like experience for running external commands and redirecting their I/O. A command's standard output can be captured, and command pipelines can be form easily.

AUTHOR
======

Jack Kuan <kjkuan@gmail.com>

COPYRIGHT AND LICENSE
=====================

Copyright 2020 Jack Kuan

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

class Shell::DSL::Proc::Async::SyncWriter
-----------------------------------------

Wraps a Proc::Async instance so that it can be used as an output file handle.

### has Shell::DSL::Pipeline $.pipeline

The Pipeline this command is part of.

### method run

```perl6
method run() returns Shell::DSL::Command:D
```

Run and return exit code.

### method capture

```perl6
method capture(
    Bool:D :$stderr = Bool::False,
    Bool:D :$check = Bool::True,
    Str:D :$encoding = "UTF-8"
) returns Str:D
```

Run and return captured STDOUT (text).

### method sink

```perl6
method sink() returns Nil
```

Run for side-effects when sunk.

class Mu $
----------

Given iterations of $p1 and $p2 below, e.g., 0,1 1,2 2,3 3,4 4,5 $p2-index goes like: 1 2 3 4 5

### sub run-pipeline

```perl6
sub run-pipeline(
    Shell::DSL::Command:D @commands where { ... }
) returns Mu
```

Set up the pipes of the Command's connected via a Pipeline. The commands are started asynchronously (via Proc::Async) with the pipe file descriptors passed over to the sub-processes via an environment variable. However, the commands are not run directly by Proc::Async, but rather via a wrapper script that knows about the file descriptor environment variable, and sets up the I/O redirections to form the pipeline before exec'ing the actual command.

### method cd

```perl6
method cd(
    $path = Code.new
) returns IO::Path:D
```

Change the execution directory for commands run via this Shell instance.

