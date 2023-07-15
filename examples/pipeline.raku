# It's also possible to pipe from one command to another.

shell {
    # A pipe is represented with the infix operator: '|>'
    .ls('-la') |> .grep('.raku$') |> .wc('-l');

    # If you pipe a Command:D to a IO::Path:D, then the standard
    # output of the command will be saved to the file.
    .ls('-la') |> .grep('.raku$') |> .wc('-l') |> 'output.txt'.IO;

    # If you'd would like to append to the file instead of overwriting
    # it, you can use the '|>>' operator.
    .ls('-la') |> .grep('.raku$') |> .wc('-l') |>> 'output.txt'.IO;

    # Use Command:D <| IO::Path:D to redirect the standard input of
    # the command from the file.
    .cat <| 'output.txt'.IO;

    # You can pipe an external command to or from a block as well.
    .find('.')
      |> pb({ .put for .lines.grep(/raku/) })
      |> .sort('-r');

    # Notice that a block within a pipeline needs to be wrapped with &pb, which
    # creates a PipeBlock instance from the block, so it can be used with '|>'.
    #
    # In the block above, .lines reads from $*IN, which in turn reads
    # from the output of 'find'. The .put writes to $*OUT, which in turn writes
    # to the pipe from which 'sort' reads.

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

    # Or, you can do something like this to save the captured results as well:
    .find('.')
      |> pb({ .put for .lines.grep(/raku/) })
      |> .sort('-r')
    andthen .capture ==> my $result;

    # The status of a pipeline is that of its last command. Because the
    # following command fails, it won't say 'found'.
    say 'found' if .echo('hello world') |> .grep('raku');

    # If you save the Pipeline instance, you can refer to each
    # indvidual Command or PipeBlock instance in the pipeline, and therefore,
    # you can get the exit status of each command in the pipeline:
    my $pipeline = (.echo('hello world') |> .grep('raku')).run;
    say "status of ({.gist}): {.rc}" given $pipeline[0];
    say "status of ({.gist}): {.rc}" given $pipeline[1];

    # This throws an exception when sunk, because 'grep' fails.
    .echo('hello world') |> .grep('raku') |> .cat;

    # This will be successful because of the :!pipefail option to &shell
    shell :!pipefail, {
        .echo('hello world') |> .grep('raku') |> .cat;  # successful
    }
}


# vim: syntax=raku ft=raku
