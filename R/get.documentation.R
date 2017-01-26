#' Get Documentation from T-SQL Metadata
#'
#' This function used R markdown to knit a PDF document of the data definition language (DDL) of any given database.
#' @param connection ODBC connection class
#' @param output.directory The directory where the documentation will land.
#' @param output.file The output name of the documentation file.
#' @keywords tsql
#' @export
#' @note Ninjas are mammals.
#' @examples
#' cnn <- get.connection('<server_name>','<database_name>')
#' doc <- get.documentation(cnn, 'c:/' , 'documentation.pdf')
get.documentation <- function(connection, output.directory, output.file) {
  
  md <- get.markdown(connection)
  
  filename <- paste(tempfile(pattern = "file", tmpdir = tempdir(), fileext = ""),'.md', sep = '')
  
  write.table(md, filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  
  documentation <- render(filename, output_format = 'pdf_document', output_dir = output.directory, output_file = output.file, quiet = TRUE)
  
  close(cnn)
  
  file.remove(filename)
  
  return(documentation)
}