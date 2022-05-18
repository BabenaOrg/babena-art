#!/bin/sh
set -e

JSON="Content-Type: application/json"
# if [ $a $eq '{}' ];
# then
#   echo 'Enter a request key'
#   exit 1
# fi

# if test -z "$a"
# then
#       echo "\$var is empty"
# else
#       echo "\$var is NOT empty"
# fi
while :
do
  # data=$(curl -H "Content-Type: application/json" -d '{"requestKeys":["'$1'"]}' -X POST https://api.testnet.chainweb.com/chainweb/0.0/testnet04/chain/0/pact/api/v1/poll)
        data=$(curl -H "Content-Type: application/json" -d '{"requestKeys":["'$1'"]}' -X POST http://localhost:8080/chainweb/0.0/development/chain/0/pact/api/v1/poll)
        echo $data
        sleep 1
done

# curl -H "Content-Type: application/json" -d '{"requestKeys":["'$1'"]}' -X POST http://localhost:8080/chainweb/0.0/development/chain/0/pact/api/v1/poll

# while :
# do
#         data=$(POST -H "Content-Type: application/json" -d '{"requestKeys":["'$1'"]}' -X POST http://localhost:8080/chainweb/0.0/development/chain/0/pact/api/v1/poll)
#         echo $data
#         sleep 1
# done
