#!/bin/bash

# extract mysql connect data from Mysqlconnect.hs
test -f mysqlconnect.data && rm -f mysqlconnect.data
head -n 14 Mysqlconnect.hs | tail -n 4 > mysqlconnect.data
echo "export mysqlhost mysqluser mysqlpasswd mysqldb" >> mysqlconnect.data
# cat mysqlconnect.data

# set envoirment to this
. mysqlconnect.data
rm -f mysqlconnect.data

fname=dump/$(date +"%F_%T").dump

mysqldump \
    -h "$mysqlhost" -u "$mysqluser" --password="$mysqlpasswd" "$mysqldb" \
    > $fname

if ( diff -q dump/latest.dump $fname )
then
    rm $fname
else
    cp $fname dump/latest.dump
    bzip2 -9 $fname
fi



