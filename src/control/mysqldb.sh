#!/bin/bash

INITFILE=autotool-init.sql

# extract mysql connect data from Mysqlconnect.hs
test -f mysqlconnect.data && rm -f mysqlconnect.data
head -n 14 Mysqlconnect.hs | tail -n 4 > mysqlconnect.data
echo "export mysqlhost mysqluser mysqlpasswd mysqldb" >> mysqlconnect.data
# cat mysqlconnect.data

# set envoirment to this 
. mysqlconnect.data
rm -f mysqlconnect.data


if [ "dump" == "$1" ]
then
  mysqldump -h "$mysqlhost" -u "$mysqluser" --password="$mysqlpasswd" "$mysqldb"
elif [ "init" == "$1" ] 
then
   echo "init database"
   echo mysql -h "$mysqlhost" -u "$mysqluser" --password="$mysqlpasswd" -D "$mysqldb" <$INITFILE
   mysql -h "$mysqlhost" -u "$mysqluser" --password="$mysqlpasswd" -D "$mysqldb" <$INITFILE
else
  echo mysql -h "$mysqlhost" -u "$mysqluser" --password="$mysqlpasswd" -D "$mysqldb" 
  mysql -h "$mysqlhost" -u "$mysqluser" --password="$mysqlpasswd" -D "$mysqldb"
fi
