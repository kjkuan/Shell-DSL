unit module IO::PipeFD;

use NativeCall;
use Fcntl;

sub c_read(int32 $fd, Pointer $buf, size_t $count --> ssize_t) is native is symbol('read') { * }
sub c_write(int32 $fd, Pointer $blob, size_t $count --> ssize_t) is native is symbol('write') { * }
sub c_close(int32 --> int32) is native is symbol('close') { * }
sub c_perror(Str) is native is symbol('perror') { * }

sub c_fcntl_void(int32 $fd, int32 $cmd --> int32) is native is symbol('fcntl') { * }
sub c_fcntl_int(int32 $fd, int32 $cmd, int32 $arg --> int32) is native is symbol('fcntl') { * }

my $ERRNO := cglobal('libc.so.6', 'errno', int32);

class X::IO::InvalidOperation is Exception is export {}

role IO::PipeFD is export {
    has $.fd of Int is required;
    has $!end-of-pipe of Bool = False;

    submethod BUILD(
        Int:D :$!fd,
        Bool :$bin, Str :$enc,
        Bool:D :$chomp=True,
        :$nl-in is raw,
        Str :$nl-out is raw
    ) {
        X::IO::BinaryAndEncoding.new.throw if $bin && $enc;
        self.encoding: $bin ?? Nil !! $enc // 'utf8';
        self.chomp = $chomp;
        self.nl-in = $nl-in if $nl-in.defined;
        self.nl-out = $nl-out if $nl-out.defined;

        # Make sure the FD doesn't have O_NONBLOCK set.
        my $flags = c_fcntl_void($!fd, Fcntl::F_GETFL);
        $flags +&= +^Fcntl::O_NONBLOCK;
        c_fcntl_int($!fd, Fcntl::F_SETFL, $flags);
    }
    method native-descriptor { $!fd }
    method opened(IO::Handle:D: --> Bool:D) { ! self.EOF }
    method close(IO::Handle:D: --> Bool:D) { c_close($!fd) == 0 }
    method open  (|)   { X::IO::InvalidOperation.new.throw }
    method lock  (|)   { X::IO::InvalidOperation.new.throw }
    method unlock(|)   { X::IO::InvalidOperation.new.throw }
    method seek  (|)   { X::IO::InvalidOperation.new.throw }
    method tell  (|)   { X::IO::InvalidOperation.new.throw }
    method flush (|)   { X::IO::InvalidOperation.new.throw }
    method WRITE (|)   { X::IO::InvalidOperation.new.throw }
    method READ  (|)   { X::IO::InvalidOperation.new.throw }
    method EOF { $!end-of-pipe }
}

class IO::PipeReadFD is IO::Handle does IO::PipeFD is export {

    method READ(IO::Handle:D: Int:D \bytes --> Buf:D) {
        my $buf = Buf.allocate(bytes);
        my $n = c_read($!fd, nativecast(Pointer[byte], $buf), bytes);
        if $n == -1 { c_perror("ERROR ($ERRNO) read($!fd, &ptr, {bytes})"); die }
        $!end-of-pipe = True if $n == 0 && bytes;
        return $buf.reallocate($n);
    }
}

class IO::PipeWriteFD is IO::Handle does IO::PipeFD is export {

    method WRITE(IO::Handle:D: Blob:D \data --> Bool:D) {
        my \bytes = data.bytes;
        my $n = c_write($!fd, nativecast(Pointer[byte], data), bytes);
        if $n == -1 { c_perror("ERROR ($ERRNO) write($!fd, &ptr, {bytes})"); die }
        return $n ≠ bytes;
        # NOTE: we consider it an error if not all bytes were written successfully.
    }
}


# vim: syntax=raku
