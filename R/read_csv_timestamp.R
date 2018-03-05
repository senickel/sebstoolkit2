#' read_csv_timestamp function
#'
#'
#' @param filepath path to file
#' @param  filename name of the file
#' @export
#' @examples
#' @importFrom data.table fread
#'
read_csv_timestamp<-function(filepath,filename) {
files<-list.files(filepath,pattern=glob2rx(paste0("*",filename,"*csv")),full.names = TRUE) %>% sort
return(fread(files[length(files)],header=TRUE))
}
