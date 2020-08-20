use v6.d;
unit module Shell::DSL:ver<0.0.1>:auth<Jack Kuan (kjkuan@gmail.com)>;

=begin pod
=NAME B<Shell::DSL> - Run and pipe commands from one to another like you would in Bash.

=DESCRIPTION
C<Shell::DSL> is a module that provides a shell-like experience for running external
commands and redirecting their I/O's. A command's standard output can be captured, and
commands can be connected to form a pipeline easily.

B<NOTE:> This module is not thread-safe.

B<Caveat:> Although an implementation detail, this module currently depends on Bash
to connect the pipes and to set up user specified I/O redirections for sub processes.

=SYNOPSIS
=begin code :lang<perl6>
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
=end code

See I<examples/> for more usage examples.

=AUTHOR Jack Kuan <kjkuan@gmail.com>

=head1 COPYRIGHT AND LICENSE
Copyright 2020 Jack Kuan

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.
=end pod


#FIXME: This is not thread-safe. Does it need to be?

#TODO:
#  - allow I/O redirections to/from IO::Path:D  with |>, |>>, <|
#    - Make it work for Command:D and Pipeline:D
#    - Make it work for PipeBlock:D
#  - allow redirecting from a string with <<<
#  - allow I/O redirection overrides when calling .run and .capture ?


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
class Pipeline { ... }

class CommandError is Exception is export {
    has $.command of Command;
    method message {
        "Command: {$!command.raku} failed with exit code {
            $!command ~~ Pipeline:D ?? $!command[*]».rc.raku !! $!command.rc
        }";
    }
}

#| A `Command:D` captures all the info (path, arguments, environment variables..., etc.)
#| needed to execute an external command. A command's PID and exit status are also
#| stored in the `Command:D` at the end of its execution.
#|
#| A `Command:D` is usually created and executed once, usually on the spot, or as part of
#| a command `Pipeline:D`. There are a few ways to execute a command:
#|
#| 1. By sinking a `Command:D`.
#| 2. By calling its `run` method directly.
#| 3. By calling its `capture` method directly.
#| 4. By evaluating a `Command:D` in a boolean context.
#| 5. By evaluating a `Command:D` in a string context.
#| 6. By evaluating a `Command:D` in a numeric context.
#|
class Command does Callable is export {
    has $.path of Str:D is required;  #= Path to the external command.
    has @.args of Str:D = ();         #= Arguments to be passed to the command.
    has %.envs is Map is required;    #= Environment variables for the command.
    has $.dir  of IO::Path = $*CWD;   #= Directory from which to execute the command from.
    has @.redirects of Pair:D;
    #= I/O redirection spec, which is a list of `Pair`s, each denotes a redirection.

    has $!rc  of Int;  # Exit code combined with signal number of the last run.
    has $.pid of Int;  # Process ID of the last run.

    has $.pipeline of Pipeline;       #= The Pipeline instance this command is part of.
    has $!async-proc of Proc::Async;

    method new(*@ (Str:D $path, *@args), :@redirects=()) {
        self.bless(:$path, :@args, :@redirects)
    }
    submethod BUILD(:$!path, :@args, :@!redirects) {
        @!args = @args».Str;
        %!envs := %*ENV.Map;
    }

    method !rc is rw { $!rc }
    method !pid is rw { $!pid }
    method !async-proc is rw { $!async-proc }
    method !pipeline is rw { $!pipeline }
    method clone(*%_ --> Command:D) {
        my $clone := callwith(|%_);
        $clone!rc = Nil;
        $clone!pid = Nil;
        $clone!pipeline = Nil;
        $clone!async-proc = Nil;
        return $clone;
    }

    method gist(--> Str:D) { self.defined ?? "$!path @!args[*]" !! nextsame }
    method raku(--> Str:D) { self.defined ?? 'Command.new' ~ ($!path, |@!args).raku !! self.^name }

    method args { return @!args.List }
    method redirects { return @!redirects.List }

    method !track-last-status(Proc:D $proc) {
        $!pid = $proc.pid;
        $!rc = $proc.exitcode;
        $!rc += $proc.signal + 128 if $proc.signal;
    }

    #| The exit status of the command's last execution.
    #| Greater than 128 means it's been killed by a signal, whose number you can get
    #| by subtracting 128 from the returned exit code.
    method rc(--> Int:D) {
        $!rc // fail "No exit code; command: {self.gist} has not yet been run!";
    }

    #| Run the command and wait for it to exit.
    method run(--> Command:D) {
        die "Invalid operation! Instead, run the pipeline the command is part of." if defined $!pipeline;
        my $proc = await Proc::Async.new(
            @!redirects ?? %?RESOURCES<cmd-exec.sh>.absolute.IO !! Empty,
            $!path, @!args
        ).start(
            :ENV(%!envs, @!redirects ?? REDIRECTS => ~translate-redirects(@!redirects) !! Empty),
            :cwd($!dir)
        );
        self!track-last-status($proc);
        return self;
    }

    #| Run the command and return captured STDOUT (text).
    method capture(
        Bool:D :$chomp=True,
        Bool:D :$check=True,
        Str:D :$encoding='utf8',
        --> Str:D
    ) {
        die "Operation not allowed! Instead, capture the pipeline the command is part of." if defined $!pipeline;
        my $proc = Proc::Async.new(
            @!redirects ??  %?RESOURCES<cmd-exec.sh>.absolute.IO !! Empty,
            $!path, @!args,
            :enc($encoding)
        );
        my @blobs;
        $proc.stdout(:bin).tap: { @blobs.push: $_ };
        $proc = await $proc.start(
            :ENV(%!envs, @!redirects ?? REDIRECTS => ~translate-redirects(@!redirects) !! Empty),
            :cwd($!dir)
        );
        my $output = Blob[byte].new(@blobs».list.flat).decode($encoding);
        self!track-last-status($proc);
        CommandError.new(:command(self)).throw if $check && $!rc ≠ 0;
        return $chomp ?? $output.chomp !! $output;
    }

    #| Same as `capture`.
    method CALL-ME(Callable:D: |c) { self.capture(|c) }

    #| Same as `capture`, except you can't pass arguments to it.
    method Str(Command:D: --> Str:D) { self.capture }

    #| Run the command if not already run, wait for it to exit, and return True if
    #| its exit status is successful; return False otherwise.
    method Bool(--> Bool:D) {
        my $self = self.run if not $!rc.DEFINITE;
        return ! $!rc;
    }

    #| Run the command if not already run, wait for it to exit, and return its exit status.
    method Numeric(--> Int:D) {
        my $self = self.run if not $!rc.DEFINITE;
        return $!rc;
    }

    #| Run the command if not already run. Throws a `CommandError` exception if the command failed.
    method sink(--> Nil) {
        my $self = self.run if not self.started;
        CommandError.new(:command(self)).throw if !self;
    }

## --- Currently these are used by Pipeline to set up I/O redirections
##     and to start the command asynchronously.
##
## These methods should be considered "protected" rather than public.
##
## Even though it's possible to declare 'trusts Pipeline;' and
## make these private but accessible to Pipeline, it seems to be more
## trouble than its worth.

    # Protected implementation detail; do not use.
    method add-to-pipeline(Pipeline:D $p --> Nil) { $!pipeline = $p }

    # Protected implementation detail; do not use.
    method async-proc(IO::Path $wrapper?, *%opts --> Proc::Async:D){
        return $!async-proc // do {
            my @cmd = $wrapper || (@!redirects ?? %?RESOURCES<cmd-exec.sh>.absolute.IO !! Empty);
            @cmd.append: $!path, |@!args;
            $!async-proc = Proc::Async.new(@cmd, |%opts);
        }
    }

    # Protected implementation detail; do not use.
    method stdout(:$bin --> Proc::Async::Pipe:D) { self.async-proc.stdout(:$bin) }

    # Protected implementation detail; do not use.
    method start(Hash() :$ENV is raw, *%opts --> Promise:D) {
        start {
            my $proc = await self.async-proc.start(
                :cwd($!dir), |%opts,
                :ENV(%!envs, $ENV, @!redirects ?? REDIRECTS => ~translate-redirects(@!redirects) !! Empty),
            );
            self!track-last-status($proc);
        }
    }

    # Protected implementation detail; do not use.
    method started(-->Bool:D) { so ($!async-proc && $!async-proc.started) }
## ---------------------------------------------------------------------------
}

class PipeBlock { ... }
subset Pipeable of Any is export where Command|PipeBlock;
#FIXME: Should Pipeable be a role instead?

#| A `Pipeline` is a `Command` made up of a sequence of `Command`'s connected
#| via pipes (see `man 2 pipe`).
#|
class Pipeline is Command does Positional is export {
    has @!parts is required of Pipeable:D handles('elems', 'AT-POS', 'EXISTS-POS');
    has @!output;  # pipeline output

    #| You should create a `Pipeline:D` via the `|>` operator instead of calling `.new`.
    method new(*@parts where { $_ > 1 && $_.all ~~ Pipeable:D }) {
        for @parts {
            .started && die "Command: {$_} has been run! It can't be part of a pipeline.";
        }
        self.bless(:path<pipeline>, :@parts);
    }

    submethod BUILD(:@!parts) { .add-to-pipeline(self) for @!parts }

    #FIXME: not sure how cloning a PipeBlock would work
    method clone(*%_ --> Pipeline:D) { nextwith(:parts(@!parts».clone), |%_) }

    method gist(--> Str:D) { self.defined ?? @!parts».gist.join(' |> ') !! nextsame }
    method raku(--> Str:D) { self.defined ?? @!parts».raku.join(' |> ') !! self.^name }

    #| Run the pipeline and wait for it to exit.
    method run(Bool :$capture, Bool :$merge --> Command:D) {
        if $capture && @!parts[*-1] !~~ Command:D {
            die "Capturing a pipeline whose last command is a PipeBlock is not supported, yet.";
        }
        # Turn "{ c1 } | c2 | c3 | c4 | { c5 }" into:  [c1, [c2, c3, c4], c5]
        # where "{ c1 }" and "{ c5 }" are PipeBlock's and c2, c3, and c4 are Command's.
        #
        # We call [c2, c3, c4] an external segment of the pipeline because it's
        # the part of the pipeline where data flow externally between the commands.
        #
        my (@pipeline, Command:D @external);

        my %external{Command:D} of Bool:D;        # when command is part of an external segment
        my %writeable{Command:D} of PipeBlock:D;  # when block needs to write to the command
        my %readable{Command:D} of PipeBlock:D;   # when block needs to read from the command

        my $p2-index = 1;
        # Given iterations of $p1 and $p2 below, e.g., 0,1  1,2  2,3  3,4  4,5
        # $p2-index goes like: 1 2 3 4 5

        for @!parts.rotor(2 => -1) -> ($p1, $p2) {
            given $p1, $p2 {
                when Command, Command {
                    # if at the start of an external segment
                    @external.push: $p1 if not @external;
                    @external.push: $p2;
                    %external{$p1} = True;
                    %external{$p2} = True;
                }
                when Command, PipeBlock {
                    # if at the end of an external segment
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
        }
        if $capture {
            # if capturing the pipeline then save the last command's stdout for reading later.
            @!output = [];
            if $merge {
                @!parts[*-1].stdout(:bin).tap: -> $blob { @!output.push: $blob };
            } else {
                @!parts[*-1].async-proc.Supply(:bin).tap: -> $blob { @!output.push: $blob };
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

    #| Run the pipeline, wait for it to exit and capture its STDOUT, which is
    #| that of the last command in the pipeline.
    method capture(
        Bool:D :$chomp=True,
        Bool:D :$check=True,
        Str:D :$encoding='utf8',
        --> Str:D
    ) {
        self.run(:capture);
        my $output = Blob[byte].new(@!output».list.flat).decode($encoding);
        @!output = [];
        return $chomp ?? $output.chomp !! $output;
    }

    # Return the exit status of the pipeline, which is that of the last command in the pipeline.
    method rc(--> Int:D) { return @!parts[*-1].rc }

    #| Run the pipeline if not already run, wait for it to exit, and return
    #| `True` if the execution is successful; return `False` otherwise.
    #|
    #| When the `:pipefail` option of `&shell` is `True` (the default), a
    #| pipeline execution is successful only if ALL commands in the pipeline
    #| exit with a `0` status code.
    #|
    #| When `:pipefail` is `False`, a pipeline execution is successful as long
    #| as the exit status of the last command in the pipeline is 0.
    method Bool(--> Bool:D) {
        my $self = self.run if not self.started;
        if $*PIPEFAIL // True {
            return @!parts».Bool.all.so;
        } else {
            return ! self.rc;
        }
    }

    #| Run the pipeline if not already run, wait for it to exit, and return its
    #| exit status, which is that of the last command in the pipeline.
    method Numeric(--> Int:D) {
        my $self = self.run if not self.started;
        return @!parts[*-1].Numeric;
    }

# #FIXME: These should match parent Command class's method signatures.
# However, currently doing Pipeline:D |> Command:D probably won't work.
#
    # Protected implementation detail; do not use.
    method async-proc(--> Proc::Async:D) { @!parts[0].async-proc }

    # Protected implementation detail; do not use.
    method stdout(:$bin --> Proc::Async::Pipe:D) { @!parts[*-1].stdout(:$bin) }

    # Protected implementation detail; do not use.
    method start(--> Promise:D) { start self.run }

    # Protected implementation detail; do not use.
    method started(--> Bool:D) { @!parts».started.any.so }
## ----------------------------------------------------------------------
}

#| A block or callable that can be used in a `Pipeline` as a filter.
class PipeBlock is export {
    has &.block;
    has $.input;
    has $.output;
    has $.pipeline of Pipeline;
    has $!promise of Promise;

    has $.encoding of Str;
    has $.chomp of Bool;
#    has @.redirects of Pair:D;

    #| You should create a `PipeBlock:D` by calling the `&pb` sub with a `Block`.
    method new(&block, Bool :$bin, Str :$enc, Bool:D :$chomp=True) {
        self.bless(:&block, :$bin, :$enc, :$chomp);
    }

    submethod BUILD(:&!block, Bool :$bin, Str :$enc, Bool:D :$!chomp) {
        X::IO::BinaryAndEncoding.new.throw if $bin && $enc;
        $!encoding = $bin ?? Nil !! $enc // 'utf8';
    }

#    method redirects { @!redirects.List }
#    method !input is rw { $!input }
#    method !output is rw { $!output }
#    method !pipeline is rw { $!pipeline }
#    method !promise is rw { $!promise }
#    method clone(*%_) {
#        my $clone = callwith(|%_);
#        $clone!input = Nil;
#        $clone!output  = Nil;
#        $clone!pipeline = Nil;
#        $clone!promise = Nil;
#        return $clone;
#    }

    method raku { 'pb { … }' }

    # Protected implementation detail; do not use.
    method add-to-pipeline(Pipeline:D $p --> Nil) { $!pipeline = $p }

    # Protected implementation detail; do not use.
    method bind-stdin(Proc::Async::Pipe:D $p) { $!input = $p } #= to read from it in the block

    # Protected implementation detail; do not use.
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

    # Protected implementation detail; do not use.
    method started(--> Bool:D) { $!promise.defined }

    method result { await $!promise }
    method rc(--> 0)                   { await $!promise }
    method Bool(PipeBlock:D: --> True) { await $!promise }
    method Numeric(PipeBlock:D: --> 0) { await $!promise }
}

# Set up the pipes of the Command's connected via a Pipeline.
#
# The commands are started asynchronously (via Proc::Async) with the
# pipe file descriptors passed over to the sub-processes via an
# environment variable. However, the commands are not run directly by
# Proc::Async, but rather via a wrapper script that knows about the
# file descriptor environment variable, and sets up the I/O redirections
# to form the pipeline before exec'ing the actual command.
#
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
        @promises.push: $cmd.start(
            :ENV(CMD_INDEX => $i,
                 PIPE_FDS  => $pipe-fds,
                 REDIRECTS => ~translate-redirects($cmd.redirects),
             )
        );
    }
    await do .async-proc.ready for @commands;

    # close the pipes
    for ^@pipes X 0,1 -> (\i, \j) {
        my $fd := @pipes[i][j];
        c_close($fd) == 0 or { c_perror "ERROR ($ERRNO): close($fd)"; die }
    }
    return @promises;
}


sub sh-quote(Str() $s --> Str:D) {
    "'{$s.subst("'", Q/'\''/)}'"
}

# Translate Shell:DSL's Command redirect notation to Bash's notation.
sub translate-redirects(@redirects) {
    my %redir-map = (
        w => '>' ,
        a => '>>',
        r => '<'
    );
    my @shell-redirects;
    for @redirects -> $spec (:$key is copy, :$value is copy) {
        my $match = $key ~~ (/^(w|a|r|to)(\d+)?$/);
        die "Invalid I/O redirection spec: {$spec.raku}" if !$match;

        my $fd;
        # Handle shortcut notation for pairs like, :2to1 (to1 => 2)
        if $key.starts-with('to') {
            $key = 'w';
            $fd = $value;
            $value = $match[1] // 1;
        } else { # $key is w|a|r
            $key = $match[0];
            $fd = $match[1] // ($key eq 'r' ?? 0 !! 1);
        }
        my $op = %redir-map{$key} // fail;
        my $quoted-value = sh-quote $value;
        @shell-redirects.push: $value ~~ Str ?? "$fd$op$quoted-value" !! "$fd$op&$quoted-value";
    }
    return @shell-redirects;
}


class CommandShell is Mu is export {
    # We inherit from Mu rather than the implicit Any so that more method names are
    # available via the FALLBACK mechanism. Particularly, 'grep' can be fallback on.

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

    method FALLBACK(Str:D $path, *@args, *%envs) {
        my @redirects = @args.grep: Pair;
        @args = @args.grep(* !~~ Pair) if @redirects;
        temp $*CWD = $!dir;
        temp %*ENV = %!env, %envs;
        Command.new($path, @args, :@redirects);
    }
}


multi sub shell(&block, Bool:D :$pipefail=True, |c --> Nil) is export {
    my $*PIPEFAIL = $pipefail;
    sink block(CommandShell.new(|c));
}
sub pb(&block --> PipeBlock:D) is export { PipeBlock.new(&block) }

sub infix:«|>»(*@operands) is assoc<list> is export {
    if @operands[*-1] ~~ IO::Path:D {
        if @operands[*-2] ~~ Command:D {
            my $path = @operands.pop;
            @operands.push: @operands.pop.clone(redirects => :w(~$path));
        } else {
            die "Redirecting PipeBlock to files not supported, yet!";
        }
    }
    return @operands[0] if @operands == 1;
    return Pipeline.new(@operands);
}

sub infix:«|>>»(Command:D $cmd, IO::Path:D $path --> Command:D) is assoc<non> is tighter(&infix:«|>») is export {
    $cmd.clone(redirects => :a(~$path));
}

multi sub postcircumfix:<{ }>(Command:D $cmd, $arg --> Command:D) is export { $cmd.clone(args => $arg) }
multi sub postcircumfix:<{ }>(Command:D $cmd, @args --> Command:D) is export { $cmd.clone(args => @args) }
multi sub postcircumfix:<{ }>(Pipeline:D $cmd, @args --> Command:D) is export { !!! }
multi sub postcircumfix:<{ }>(Pipeline:D $cmd, @args --> Command:D) is export { !!! }



# vim: syntax=perl6 ft=perl6
