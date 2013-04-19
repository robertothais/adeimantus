#!/bin/bash

source ./require.sh

cp $IPATH/config/ejabberd.cfg /etc/ejabberd/ejabberd.cfg
cp $IPATH/config/ejabberd_default /etc/default/ejabberd
echo "ERLANG_NODE=ejabberd@`local_hostname`" >> /etc/default/ejabberd