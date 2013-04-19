#!/bin/bash

source ./require.sh


# Stop ejabberd
service ejabberd stop

sleep 5

# Append limits
cat $IPATH/config/limits.conf >> /etc/security/limits.conf

# Compile and install vendored and custom modules
compile_modules_in_directory $IPATH/erlang

# Update cookie
export EJABBERD_COOKIE=NFTLBDZWGYSVDIACABHA
echo "$EJABBERD_COOKIE" > /var/lib/ejabberd/.erlang.cookie
chown ejabberd.ejabberd /var/lib/ejabberd/.erlang.cookie

chmod 400 /var/lib/ejabberd/.erlang.cookie

# Update config files
./update_ejabberd_config.sh

# Updating the config the first time will change the node name
# We need to clear the Mnesia spool files
rm -rf /var/lib/ejabberd/*

# Start ejabberd
service ejabberd start