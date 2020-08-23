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
    # 'cd' is a "built-in" method of the Shell class that changes
    # the invocation directory of external commands.
    .cd: $*HOME;

    # A command instance, when sunk, runs the command. Running a command
    # returns the command instance too. The command instance tracks the pid
    # exit status of the command run. 
    .pwd;

    # Command arguments can be passed to the command as positional arguments.
    .ls: '-la'; 

    # You can also pass the arguments to a command by indexing it like a hash.
    .ls<-l -a>;

    # Any named arguments will be set as environment variables for the command.
    .bash: «-c 'echo $GREETING'» , :GREETING<Hello>;

    # A sunk command instance throws an exception if it exited with a non-zero status.
    .bash: «-c 'exit 123'» ;    # exception

    # Evaluating a command in a boolean context runs it if it hasn't been run,
    # and returns True if it exited successfully or False, otherwise.
    say "Command Failed!" unless .bash(«-c 'exit 1'»);

    # You can also run a command explicitly via its 'run' method.
    my $cmd = .echo('Hello').run;  # Hello
    say $cmd.rc;                   # 0 (exit status)
    say +$cmd;                     # same thing

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
    .echo('hello') eq 'hello';

    # Also note that capturing, by default, chomps the trailing newline.
    # To keep the trailing newline:
    .echo('hello').(:!chomp) eq "hello\n";

    # To capture the STDERR, you can redirect it to STDOUT like this:
    say .bash(«-c 'echo hello >&2'», (:2to1)).();

    # Notice that the parenthesis around :2to1 is need to keep it as a Pair.
    # Also, :2to1 is a shortcut for :w2(1). It's possible to redirect I/O
    # from/to files. Here are some example redirection specs:
    #
    #   # Redirects standard output to /tmp/output.txt
    #   (:w</tmp/out.txt>)  # Bash: >/tmp/out.txt
    #
    #   # Redirects both standard output and standard error to /tmp/out.txt.
    #   (:w</tmp/out.txt>, :w2(1))  # Bash: >/tmp/out.txt 2>&1
    #
    #   # Append standard output to /tmp/out.txt, but redirect standard error to /dev/null
    #   (:a</tmp/out.txt>, :w2</dev/null>)  # Bash: >> /tmp/out.txt 2>/dev/null
    #
    #   # Redirect standard input to read from /tmp/data.txt
    #   (:r</tmp/data.txt>)  # Bash: < /tmp/data.txt
    #
    # Shortcuts exist for redirecting standard output to a file or for
    # redirecting standard input from a file. See pipeline.raku for examples.

    # Capturing, by default, also checks the command exit status and throws
    # an exception if the command has failed.
    say .bash(<-c 'echo hello; exit 1'>).();  # You will get an exception.

    # If you only want the captured output and don't care about exit status
    # then you can disable the check like this:
    my $greeting-cmd = .bash(«-c 'echo hello; exit 1'»);
    say $greeting-cmd.(:!check);  # hello
    say $greeting-cmd.rc;         # 1
}

# vim: syntax=perl6 ft=perl6
