#!/bin/bash

source ./require.sh

# Stop ejabberd
/etc/init.d/ejabberd stop

sleep 3

nodes=($EJABBERD_NODES)
echo "Joining cluster with $nodes"

# Clear Mnesia files
rm -rf /var/lib/ejabberd/*

echo "Running erl -pa /usr/lib/ejabberd/ebin -name ejabberd@`local_hostname` -s adeimantus_admin add_node ${nodes[0]} -run init stop -oshell"
su ejabberd -l -c "exec erl -pa /usr/lib/ejabberd/ebin \
  -mnesia dir '\"/var/lib/ejabberd\"' \
  -name ejabberd@$`local_hostname` \
  -s adeimantus_admin add_node ${nodes[0]} \
  -run init stop -noshell"

/etc/init.d/ejabberd start