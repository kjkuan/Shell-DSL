#!/usr/bin/env bash
#
#FIXME: Is it worth it to implement it C?
#
set -fe

# command index:   0    1    2
#                 (c1 | c2 | c3)
#    pipe index:      0    1
#

# Pairs of comma separated FDs delimited by spaces.
PIPE_FDS=($PIPE_FDS)  # e.g., (3,4 5,6 7,8)

# If not the first command, redirect its STDIN to the read end of the pipe.
if [[ $CMD_INDEX != 0 ]]; then
    fds=${PIPE_FDS[$CMD_INDEX - 1]}
    eval "exec 0<&${fds%,*}"
fi

# If not the last command, redirect its STDOUT to the write end of the pipe.
if [[ $CMD_INDEX != ${#PIPE_FDS[*]} ]]; then
    fds=${PIPE_FDS[$CMD_INDEX]}
    eval "exec 1>&${fds#*,}"
fi

# Close the FDs now that we have done the redirections.
for fds in ${PIPE_FDS[*]}; do
    eval "exec ${fds%,*}>&- ${fds#*,}>&-"
done
    
exec "$@"
