#!/bin/bash

source ./require.sh

su ejabberd -l -c "exec erl -pa /usr/lib/ejabberd/ebin \
  -mnesia dir '\"/var/lib/ejabberd\"' \
  -name codereload@`local_hostname` \
  -s adeimantus_admin hot_reload ejabberd@`local_hostname` \
  -run init stop -noshell"
