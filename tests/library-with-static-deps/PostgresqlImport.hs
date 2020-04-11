module PostgresqlImport where

import Database.PostgreSQL.LibPQ

aDbConnectionStatus :: ConnStatus
aDbConnectionStatus = ConnectionBad
