INITFILE=autotool-init.sql

# extract mysql connect data from Mysqlconnect.hs
test -f mysqlconnect.data && rm -f mysqlconnect.data
head -n 9 Mysqlconnect.hs | tail -n 4 > mysqlconnect.data
echo "export mysqlhost mysqluser mysqlpasswd mysqldb" >> mysqlconnect.data
# cat mysqlconnect.data

# set envoirment to this 
. mysqlconnect.data
rm -f mysqlconnect.data


if [ "init" == "$1" ] 
then
   echo "init database"
   mysql -h "$mysqlhost" -u "$mysqluser" --password="$mysqlpasswd" -D "$mysqldb" <$INITFILE
else
   mysql -h "$mysqlhost" -u "$mysqluser" --password="$mysqlpasswd" -D "$mysqldb"
fi