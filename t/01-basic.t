use v6.d;
use Test;
use Shell::DSL;

shell {
    my $tmpfile = "/tmp/{rand.split('.')[1]}.txt";
    ok .touch: $tmpfile;
    ok $tmpfile.IO.e;

    is .echo('hello'), 'hello';
    is .echo(<-n hello>), 'hello';

    ok .cd: '/tmp';
    ok .test: «-e "{$tmpfile.IO.basename}"»;

    .echo('hello', (:w($tmpfile)));
    .echo('world', (:a($tmpfile)));
    is .cat((:r($tmpfile))), "hello\nworld";

    .echo«"abc\ndef\nghi"»  |> .cat |> pb({ .put for .lines}) |> .sed((:w($tmpfile)), 's/^/x /');
    is .cat((:r($tmpfile))) |> .tr(<x z>), "z abc\nz def\nz ghi";

    ok .cd;
    is .pwd, $*HOME;

    is .bash(«-c 'echo $SECRET'», :SECRET<abc123>), 'abc123';
    is .bash(«-c 'echo line1; echo line2 >&2'», (:2to1)).(), "line1\nline2";

    my $cmd = .bash(«-c 'echo blah blah blah; exit 1'»);
    dies-ok $cmd;
    is $cmd.(:!check), 'blah blah blah';
    is $cmd.rc, 1;

    my $output;
    ok .echo('hello')
       |> pb({ print .slurp })
       |> .cat()
       |> pb({ $output = .get });
    is $output, "hello";

    ok .cd: '/tmp';
    my $name = $tmpfile.IO.basename;
    for $name xx 5 Z~ 1..5 -> $file {
        .touch: $file;
    }
    is .ls('-la') |> .grep($name) |> .wc('-l'), 6;

    my $last-line;
    ok .ls('-la') |> .grep($name) |> pb({
           for .lines().kv -> $i, $line {
               put "xxx $line" if $i !%% 2;
           }
        }) |> .tail(-1)
           |> pb({ $last-line = .get })
        ;
    ok $last-line ~~ /^xxx/;
    is $last-line.split(/' '+/)[*-1], "{$name}5";

    is (pb({ $*OUT.print: 'H4sIAII2Ll8AA8tIzcnJVyjPL8pJAQCFEUoNCwAAAA==' })
        |> .base64('-d')
        |> .gunzip('-c')
        |> pb({ .slurp.uc.print })
        |> .gzip('-nc')
        |> .md5sum
        |> .cut(«-d' ' -f1»)
       ).capture, '06677e1754007e6bf84b1415fdc28e5c';

    dies-ok { .echo('hello world') |> .grep('raku') |> .cat };
    lives-ok { shell(:!pipefail, { .echo('hello world') |> .grep('raku') |> .cat }) };

    LEAVE {
        .find: «/tmp -maxdepth 1 -mindepth 1 -name "$name*" -delete»; 
    }
}

done-testing;


# vim: syntax=perl6 ft=perl6
