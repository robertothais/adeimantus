#!/bin/bash

IPATH=/home/ubuntu/adeimantus/current/instance

EJABBERD_INCLUDE_DIR=/usr/lib/ejabberd/include
EJABBERD_MODULE_DIR=/usr/lib/ejabberd/ebin

compile_modules_in_directory() { 
  for file in `ls $1/src | egrep \.erl$`; do     
    local compiled="`basename $file .erl`.beam"
    erlc -I $EJABBERD_INCLUDE_DIR -I $1/include $1/src/$file
    chmod 644 $compiled
    chown ejabberd.ejabberd $compiled
    mv $compiled $EJABBERD_MODULE_DIR
  done
}

local_hostname() {
  curl --silent http://169.254.169.254/latest/meta-data/local-hostname
}