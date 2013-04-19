#!/bin/bash

source ./require.sh

su ubuntu -c "cat $IPATH/keys/adeimantus.pub > ~/.ssh/authorized_keys"
curl --silent http://169.254.169.254/latest/meta-data/public-keys/0/openssh-key >> ~/.ssh/authorized_keys