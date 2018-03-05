#' read_csv_version function
#'
#' Reads versioned file. Corresponding to write_csv_version
#' @param filepath path to file
#' @param filename name of file
#' @keywords
#' @export
#' @examples
#' @importFrom data.table fread
#'
read_csv_version<-function(filepath,filename) {
  library(data.table)
  vc<-version_counter(filepath,filename,".csv",0)
  return(fread(paste0(filepath,filename,vc,".csv"),header = TRUE))
}
