#!/usr/bin/env bash
set -e

# User specified I/O redirections
if [[ $REDIRECTS ]]; then
    eval exec "$REDIRECTS"
fi
unset -v REDIRECTS

exec "$@"
