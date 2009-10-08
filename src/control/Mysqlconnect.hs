module Mysqlconnect where 

import Database.HSQL.MySQL

-- Mysql
mysqlhost="localhost"
mysqldb="autosnap"
mysqluser="autosnap"
mysqlpasswd="nomic"

myconnect = connect mysqlhost mysqldb mysqluser mysqlpasswd 
