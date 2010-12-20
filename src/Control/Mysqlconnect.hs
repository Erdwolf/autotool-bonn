module Mysqlconnect where 

import Database.HSQL.MySQL

-- Mysql
mysqlhost="localhost"
mysqldb="autoan"
mysqluser="user"
mysqlpasswd="passwort"

myconnect = connect mysqlhost mysqldb mysqluser mysqlpasswd 
