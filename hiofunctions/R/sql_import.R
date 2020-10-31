# SQL_import: import from SQL database
sql_import <- function(statement, dbname = "GLOBALIO", outname) {

  connection <- dbConnect(RMariaDB::MariaDB(),
                          user = 'jus3',
                          password = '$eam0n$teR',
                          dbname = dbname)

  web <- dbSendQuery(connection, statement)
  web <- dbFetch(web)
  dbDisconnect(connection)

  assign(outname, web, envir = parent.frame())
}
