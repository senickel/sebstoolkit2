#' read_csv_version function
#'
#' check ordinal if they are ordinal
#' @param Click here and there
#' @keywords ordinal
#' @keywords
#' @export
#' @examples
#' get.1.sub(data,split,cols,whichcol,replace)
#'
read_csv_version<-function(filepath,filename) {
  library(data.table)
  vc<-version_counter(filepath,filename,".csv",0)
  return(fread(paste0(filepath,filename,vc,".csv")))
}
