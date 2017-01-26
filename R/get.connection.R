#' Get Connection Class for Documenter
#'
#' This function returns an ODBC connection class to a T-SQL Server.
#' @param server Server (Instance) Name
#' @param catalog Database Name
#' @keywords tsql
#' @export
#' @examples
#' cnn <- get.connection('<server_name>','<database_name>')
get.connection <- function(server, catalog) {
  
  cns <- paste('driver={SQL Server};server=',server,';database=',catalog,';trusted_connection=true', sep = '')

  cnn <- odbcDriverConnect(cns)

  return(cnn)
}