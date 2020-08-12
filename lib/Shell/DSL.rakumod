use v6.d;
unit module Shell::DSL:ver<0.0.1>:auth<Jack Kuan (kjkuan@gmail.com)>;

=begin pod

=head1 NAME
Shell::DSL - Run commands and pipe from one to another like you would in a command shell.


=head1 SYNOPSIS

=begin code :lang<perl6>

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


=end code

=head1 DESCRIPTION

C<Shell::DSL> is a module that provides a shell-like experience for running external
commands and redirecting their I/O. A command's standard output can be captured, and
command pipelines can be form easily.

=head1 AUTHOR

Jack Kuan <kjkuan@gmail.com>

=head1 COPYRIGHT AND LICENSE

Copyright 2020 Jack Kuan

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod

#FIXME: This is not thread-safe. Does it need to be?

#TODO:
#  - allow I/O redirections to/from files or pathes  with <, >, <<, >> 


use NativeCall;
use IO::PipeFD;

#sub c_dup2(int32 $oldfd, int32 $newfd) is native is symbol('dup2') { * }
sub c_pipe(CArray[int32] --> int32) is native is symbol('pipe') { * }
sub c_close(int32 --> int32) is native is symbol('close') { * }
sub c_perror(Str) is native is symbol('perror') { * }

my $ERRNO := cglobal('libc.so.6', 'errno', int32);


#| Wraps a Proc::Async instance so that it can be used as an output file handle.
#|
class Proc::Async::SyncWriter is IO::Handle is export {
    has $.proc of Proc::Async is required where .w;

    submethod BUILD(
        Proc::Async:D :$!proc,
        Bool :$bin,
        Str :$enc,
        Str :$nl-out
    ) {
        X::IO::BinaryAndEncoding.new.throw if $bin && $enc;
        self.encoding($bin ?? Nil !! $enc // 'utf8');
        self.nl-out = $nl-out if $nl-out.defined;
    }

    method open  (|)   { X::IO::InvalidOperation.new.throw }
    method lock  (|)   { X::IO::InvalidOperation.new.throw }
    method unlock(|)   { X::IO::InvalidOperation.new.throw }
    method seek  (|)   { X::IO::InvalidOperation.new.throw }
    method tell  (|)   { X::IO::InvalidOperation.new.throw }
    method flush (|)   { X::IO::InvalidOperation.new.throw }
    method READ  (|)   { X::IO::InvalidOperation.new.throw }

    method WRITE(IO::Handle:D: Blob:D \data --> Bool:D) {
        try await $!proc.write(data);
        return $! ?? False !! True;
    }
    method close(--> Bool:D)    { $!proc.close-stdin }
    method ready(--> Promise:D) { $!proc.ready }
    method opened(--> Bool:D)   { so await self.ready }
}


class Command { ... }
class CommandError is Exception is export {
    has Command $.command;
    method message {
        "Command: {$!command.raku} failed with exit code {$!command.rc}";
    }
}

class Pipeline { ... }

class Command does Callable is export {
    has $.path of Str:D is required;
    has @.args of Str:D = ();
    has %.envs is Map is required;
    has $.dir  of IO::Path = $*CWD;
    has $.io-redirects of Str;    # FIXME: arbitrary FD redirects not implemented yet...

    has $!rc  of Int;  # exit code combined with signal number of the last run.
    has $.pid of Int;  # of the last run.

    #| The Pipeline this command is part of.
    has $.pipeline of Pipeline;
    has $!async-proc of Proc::Async;

    method new(*@ (Str:D $path, *@args), Str :$io-redirects) {
        self.bless(:$path, :@args, :$io-redirects)
    }
    submethod BUILD(:$!path, :@!args, :$!io-redirects) { %!envs := %*ENV.Map }

    method gist(--> Str:D) { self.defined ?? "$!path @!args[*]" !! nextsame }
    method raku(--> Str:D) { self.defined ?? 'Command.new' ~ ($!path, |@!args).raku !! self.^name }

    method !track-last-status(Proc:D $proc) {
        $!pid = $proc.pid;
        $!rc = $proc.exitcode;
        $!rc += $proc.signal + 128 if $proc.signal;
    }

    method rc(--> Int:D) {
        $!rc // fail "No exit code; command: {self.gist} has not yet been run!";
    }

    #| Run and return exit code.
    method run(--> Command:D) {
        die "Invalid operation! Instead, run the pipeline the command is part of." if defined $!pipeline;
        my $proc = run($!path, @!args, :env(%!envs), :cwd($!dir));
        self!track-last-status($proc);
        return self;
    }

    #| Run and return captured STDOUT (text).
    method capture(
        Bool:D :$stderr=False,
        Bool:D :$check=True,
        Str:D :$encoding='UTF-8',
        --> Str:D
    ) {
        die "Operation not allowed! Instead, capture the pipeline the command is part of." if defined $!pipeline;
        my $proc = Proc.new(:out, :merge($stderr), :!bin, :!chomp, :enc($encoding));
        $proc.spawn($!path, @!args, :env(%!envs), :cwd($!dir));
        my $output = $proc.out.slurp(:close).chomp;
        self!track-last-status($proc);
        CommandError.new(:command(self)).throw if $check && $!rc ≠ 0;
        return $output;
    }
    method CALL-ME(Callable:D: |c)   { self.capture(|c) }
    method Str(Command:D: --> Str:D) { self.capture     }

    method Bool(--> Bool:D) {
        self.run if not $!rc.DEFINITE;
        return ! $!rc;
    }
    method Numeric(--> Int:D) {
        self.run if not $!rc.DEFINITE;
        return $!rc;
    }

    #| Run for side-effects when sunk.
    method sink(--> Nil) {
        my $self = self.run if not self.started;
        CommandError.new(:command(self)).throw if !self;
    }

## --- Currently these are used by Pipeline to set up I/O redirections
##     and to start the command asynchronously.
## --- These methods should be considered "protected" rather than public.
##
    method add-to-pipeline(Pipeline:D $p --> Nil) { $!pipeline = $p }
    method async-proc(IO::Path $wrapper?, *%opts --> Proc::Async:D){
        return $!async-proc // do {
            my @cmd = $wrapper || Empty;
            @cmd.append: $!path, |@!args;
            $!async-proc = Proc::Async.new(@cmd, |%opts);
        }
    }
    method stdout(:$bin --> Proc::Async::Pipe:D) { self.async-proc.stdout(:$bin) }
    method start(Hash() :$ENV is raw, *%opts --> Promise:D) {
        start {
            my $proc = await self.async-proc.start(:cwd($!dir), |%opts, :ENV(%!envs, $ENV));
            self!track-last-status($proc);
        }
    }
    method started(-->Bool:D) { so ($!async-proc && $!async-proc.started) }
## ---------------------------------------------------------------------------
}

class PipeBlock { ... }
subset Pipeable of Any is export where Command|PipeBlock;
#FIXME: Should Pipeable be a role instead?

class Pipeline is Command is export {
    has @!parts is required of Pipeable:D;
    has @!output;  # pipeline output

    method new(*@parts where { $_ > 1 && $_.all ~~ Pipeable:D }) {
        for @parts {
            .started && die "Command: {$_} has been run! It can't be part of a pipeline.";
        }
        self.bless(:path<pipeline>, :io-redirects(Str), :@parts);
    }

    submethod BUILD(:@!parts) { .add-to-pipeline(self) for @!parts }
    method postcircumfix:<[ ]>(Int:D $i --> Pipeable) { return @!parts[$i] }

    method gist(--> Str:D) { self.defined ?? @!parts».gist.join(' |> ') !! nextsame }
    method raku(--> Str:D) { self.defined ?? @!parts».raku.join(' |> ') !! self.^name }

    method run(Bool :$capture, Bool :$merge --> Command:D) {
        #| Turn "{ c1 } | c2 | c3 | c4 | { c5 }" into:  [c1, [c2, c3, c4], c5]
        #| where "{ c1 }" and "{ c5 }" are PipeBlock's and c2, c3, and c4 are Command's.
        #|
        #| We call [c2, c3, c4] an external segment of the pipeline because it's
        #| the part of the pipeline where data flow externally between the commands.
        #|
        my (@pipeline, Command:D @external);

        my %external{Command:D} of Bool:D;        # when command is part of an external segment
        my %writeable{Command:D} of PipeBlock:D;  # when block needs to write to the command
        my %readable{Command:D} of PipeBlock:D;   # when block needs to read from the command

        my $p2-index = 1;
        #= Given iterations of $p1 and $p2 below, e.g., 0,1  1,2  2,3  3,4  4,5
        #= $p2-index goes like: 1 2 3 4 5

        for @!parts.rotor(2 => -1) -> ($p1, $p2) {
            given $p1, $p2 {
                when Command, Command {
                    #| if at the start of an external segment
                    @external.push: $p1 if not @external;
                    @external.push: $p2;
                    %external{$p1} = True;
                    %external{$p2} = True;
                }
                when Command, PipeBlock {
                    #| if at the end of an external segment
                    if @external {
                        @pipeline.push: @external.clone;
                        @external = [];
                    } else {
                        @pipeline.push: $p1 if !@pipeline || @pipeline[*-1] !=== $p1;
                    }
                    @pipeline.push: $p2;
                    %readable{$p1} = $p2;
                }
                when PipeBlock, Command {
                    @pipeline.push: $p1 if !@pipeline || @pipeline[*-1] !=== $p1;
                    @pipeline.push: $p2 if @!parts[$p2-index+1] !~~ Command;
                    %writeable{$p2} = $p1;
                }
                when PipeBlock, PipeBlock {
                    !!!  #FIXME: Allowing this would be interesting but not high priority right now.
                }
            }
            ++$p2-index;
        }
        @pipeline.push: @external if @external;

        my $pipe-exec = %?RESOURCES<pipe-exec.sh>.absolute.IO;
        #$pipe-exec.chmod(0o0755) if not $pipe-exec.IO.x;

        for @!parts.grep: Command:D -> $cmd {
            # Create Proc::Async for each Command object
            $cmd.async-proc(
                # Those that are part of an external segment will be run with the
                # $pipe-exec wrapper to set up the I/O redirections with the pipes.
                | (%external{$cmd} ?? $pipe-exec !! Empty),
                :w(%writeable{$cmd}.defined)
            );
            # Set up I/O redirections for the PipeBlock connected to this Command
            .bind-stdin($cmd.stdout) with %readable{$cmd};  # Command | { ... }
            with %writeable{$cmd} {                         # { ... } | Command
                my $out = Proc::Async::SyncWriter.new(:proc($cmd.async-proc));
                .bind-stdout($out);
            }
            # if we are capturing the pipeline output and $cmd is the last
            # command of the pipeline then save its stdout for reading later.
            if $capture and $cmd === @!parts[*-1] {
                @!output = [];
                if $merge {
                    $cmd.stdout(:bin).tap: -> $blob { @!output.push: $blob };
                } else {
                    $cmd.async-proc.Supply(:bin).tap: -> $blob { @!output.push: $blob };
                }
            }
        }

        my @promises;
        for @pipeline {
            when Array:D { @promises.append: run-pipeline($_) }
            default      { @promises.push: .start             }
        }
        await @promises;
        return self;
    }

    method capture(
        Bool:D :$stderr=False,
        Bool:D :$check=True,
        Str:D :$encoding='UTF-8',
        --> Str:D
    ) {
        self.run(:capture, :merge($stderr));
        my $output = Blob[byte].new(@!output».list.flat).decode($encoding);
        @!output = [];
        return $output.chomp;
    }

    method rc(--> Int:D) {
        self.run if not self.started;
        return @!parts[*-1].rc;
    }
    method Bool(--> Bool:D) {
        self.run if not self.started;
        return @!parts[*-1].Bool;
        #FIXME: Add an option to cause returning @!parts.any.Bool here instead.
        #       i.e., like 'set -o pipefail' in Bash.
    }
    method Numeric(--> Int:D) {
        self.run if not self.started;
        return @!parts[*-1].Numeric;
    }

## --- These methods should be considered "protected" rather than public.
    method async-proc(--> Proc::Async:D) { @!parts[0].async-proc }
    method stdout(:$bin --> Proc::Async::Pipe:D) { @!parts[*-1].stdout(:$bin) }
    method start(--> Promise:D) { start self.run }
    method started(--> Bool:D) { @!parts».started.any.so }
## ----------------------------------------------------------------------
}

# A block or callable that can be used in a pipeline.
# This is not inherited from Command since it's really different enough
# that it deserve its own class.
#
class PipeBlock is export {
    has &.block;
    has $.input;
    has $.output;
    has $.pipeline of Pipeline;
    has $!promise of Promise;

    has $.encoding of Str;
    has $.chomp of Bool;

    method new(&block, Bool :$bin, Str :$enc, Bool:D :$chomp=True) {
        self.bless(:&block, :$bin, :$enc, :$chomp);
    }

    submethod BUILD(:&!block, Bool :$bin, Str :$enc, Bool:D :$!chomp) {
        X::IO::BinaryAndEncoding.new.throw if $bin && $enc;
        $!encoding = $bin ?? Nil !! $enc // 'utf8';
    }

    method raku { 'pb { … }' }

    method add-to-pipeline(Pipeline:D $p --> Nil) { $!pipeline = $p }
    method bind-stdin(Proc::Async::Pipe:D $p) { $!input = $p } #= to read from it in the block
    method bind-stdout(IO::Handle:D $out) { $!output = $out }  #= to write to it in the block

    method start(--> Promise:D) {
        my \STDIN = $*IN;
        my \STDOUT = $*OUT;
        my $*SELF := self;
        $!promise = start sub {
            my $*IN = STDIN;
            with $!input {
                my $fd = await $!input.native-descriptor;
                $*IN = IO::PipeReadFD.new(fd => $fd, :$!chomp, :enc($!encoding));
            }
            my $*OUT = STDOUT;
            with $!output {
                await $!output.ready if $!output ~~ Proc::Async::SyncWriter:D;
                $!output.encoding($!encoding);
                $*OUT = $!output;
            }
            my $result = try &!block($*IN);
            $result = Failure.new: $! if $!;

            $*IN.close if defined $!input;
            $*OUT.close if defined $!output;

            return $result;
        }();
    }
    method run(--> PipeBlock:D) {
        die "Invalid operation! Instead, run the pipeline the block is part of." if defined $!pipeline;
        await self.start;
        return self;
    }
    method result { await $!promise }

    method started(--> Bool:D) { $!promise.defined }
    method rc(--> 0)                   { await $!promise }
    method Bool(PipeBlock:D: --> True) { await $!promise }
    method Numeric(PipeBlock:D: --> 0) { await $!promise }
}

#| Set up the pipes of the Command's connected via a Pipeline.
#|
#| The commands are started asynchronously (via Proc::Async) with the
#| pipe file descriptors passed over to the sub-processes via an
#| environment variable. However, the commands are not run directly by
#| Proc::Async, but rather via a wrapper script that knows about the
#| file descriptor environment variable, and sets up the I/O redirections
#| to form the pipeline before exec'ing the actual command.
#|
sub run-pipeline(Command:D @commands where * > 1) {
    my @pipes;
    my $pipe-fds = [];
    for ^(@commands - 1) -> $i {
        @pipes[$i] := CArray[int32].new(0, 0);
        c_pipe(@pipes[$i]) == 0 or { c_perror "ERROR ($ERRNO) pipe()"; die }
        $pipe-fds.push: @pipes[$i].join(',');
    }
    $pipe-fds = $pipe-fds.join(' ');

    my @promises;
    for @commands.kv -> $i, $cmd {
        @promises.push: $cmd.start(:ENV(CMD_INDEX => $i, PIPE_FDS => $pipe-fds));
    }
    await do .async-proc.ready for @commands;

    # close the pipes
    for ^@pipes X 0,1 -> (\i, \j) {
        my $fd := @pipes[i][j];
        c_close($fd) == 0 or { c_perror "ERROR ($ERRNO): close($fd)"; die }
    }
    return @promises;
}


class CommandShell is Mu is export {
    #= We inherit from Mu rather than the implicit Any so that more method names are
    #= available via the FALLBACK mechanism. Particularly, 'grep' can be fallback on.

    has $.dir of IO::Path:D = $*CWD;
    has %.env of Str:D      = %*ENV;

    submethod BUILD(:$dir, :$env) {
        $!dir = $dir if defined $dir;
        %!env = $env if defined $env;
    }

    #| Change the execution directory for commands run via this Shell instance.
    method cd(IO() $path=$*HOME --> IO::Path:D) {
        fail "Directory '$path' does not exist!" if !$path.d;
        $!dir = $path.absolute.path;
    }

    method export(*%env-vars) {
        %!env ,= %env-vars;
    }

    method FALLBACK(Str:D $path, *@args, *%envs, Str :$fd) {
        temp $*CWD = $!dir;
        temp %*ENV = %!env, %envs;
        @args := @args».Str;
        Command.new($path, @args, :io-redirects($fd));
    }
}


multi sub shell(&block, |c) is export { block(CommandShell.new(|c)) }
sub pb(&block --> PipeBlock:D) is export { PipeBlock.new(&block) }
sub infix:«|>»(*@blocks --> Pipeline:D) is assoc<list> is export { Pipeline.new(@blocks) }





# vim: syntax=perl6 ft=perl6
