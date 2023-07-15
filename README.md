NAME
====
`Shell::DSL` - Run and pipe commands from one to another like you would in Bash.

DESCRIPTION
===========
`Shell::DSL` is a module that provides a shell-like experience for running
external commands and redirecting their I/O's. A command's standard output can
be captured, and commands can be connected to form a pipeline easily.

**NOTE:** This module is not thread-safe.

**Caveat:** I've only tested it on Ubuntu, but it should work on any Unix-like
system with the right native library dependencies. Moreover, although an
implementation detail, this module currently depends on Bash to connect the
pipes and to set up user specified I/O redirections for sub processes.

SYNOPSIS
========
```raku
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

See [examples/](examples/) for more usage examples.
