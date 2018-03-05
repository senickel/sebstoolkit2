#' read_csv_timestamp function
#'
#' check ordinal if they are ordinal
#' @param Click here and there
#' @keywords ordinal
#' @keywords
#' @export
#' @examples
#' get.1.sub(data,split,cols,whichcol,replace)
#'
read_csv_timestamp<-function(filepath,filename) {
  library(data.table)
files<-list.files(filepath,pattern=glob2rx(paste0("*",filename,"*csv")),full.names = TRUE) %>% sort
return(fread(files[length(files)]))
}
