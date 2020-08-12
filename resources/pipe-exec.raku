#!/usr/bin/env raku
#
# This is implemented after implementing pipe-exec.sh to see how well it will work.
# Too bad, it's way slower than pipe-exec.sh, so it's not used but only included as a proof
# of concept.
#
use NativeCall;
use File::Which;

sub c_close(int32) returns int32 is native is symbol('close') { * }
sub c_dup2(int32, int32) returns int32 is native is symbol('dup2') { * }
sub c_execve(Str, CArray[Str], CArray[Str]) returns int32 is native is symbol('execve') { * }
sub c_signal(int32, long) is native is symbol('signal') { * }
sub c_perror(Str) is native is symbol('perror') { * }
sub c_strerror(int32) returns Str is native is symbol('strerror') { * }

my $ERRNO := cglobal('libc.so.6', 'errno', int32);
constant ENOENT = 2;
constant EACCES = 13;


sub exec(*@ ($path, *@args), Hash() :$env = %*ENV) {
    my @argvs := CArray[Str].new($path, @args, Str);
    my @env := CArray[Str].new($env.kv.map({"$^k=$^v"}), Str);
    c_execve($path, @argvs, @env);
}

# Use the default signal handler for SIGPIPE.
c_signal(+SIGPIPE, 0);

my @PIPE_FDS = « %*ENV<PIPE_FDS> ».map(*.split(','))».Int;
my $i = %*ENV<CMD_INDEX>;

#FIXME: handle user specified I/O redirects such as 2>&1 or 2>/dev/null

%*ENV<PIPE_FDS>:delete;
%*ENV<CMD_INDEX>:delete;

# If not the first command, redirect its STDIN to the read end of the pipe
if $i ≠ 0 {
    c_dup2(@PIPE_FDS[$i-1][0], 0) == 0 or { c_perror("Input redirection failed"); die }
}

# If not the last command, redirect its STDOUT to the write end of the pipe
if $i ≠ @PIPE_FDS {
    c_dup2(@PIPE_FDS[$i][1], 1) == 0 or { c_perror("Output redirection failed"); die }
}

for @PIPE_FDS -> (Int $r, Int $w) {
    c_close($r) or { c_perror("Failed closing pipe FD (read): $r"); die }
    c_close($w) or { c_perror("Failed closing pipe FD (write): $w"); die }
}

my $prog = which(@*ARGS[0]) // exit 127;
exec $prog, |@*ARGS[1..*];

# exec must have failed…
my $errno = $ERRNO;
note "Failed executing '$prog': {c_strerror($errno)}";
exit 126 if $errno == EACCES;
exit 127 if $errno == ENOENT;
exit 1;

# vim: ft=perl6
