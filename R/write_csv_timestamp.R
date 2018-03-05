#' write_csv_timestamp function
#' 
#' check ordinal if they are ordinal
#' @param Click here and there
#' @keywords ordinal
#' @keywords
#' @export
#' @examples 
#' get.1.sub(data,split,cols,whichcol,replace)

write_csv_timestamp<-function(data,filepath,filename) {
  library(dplyr)
  library(data.table)
  ts<-Sys.time() %>% format(.,"%Y-%m-%d")
  filepath<-paste0(filepath,filename,ts,".csv")
  fwrite(data,filepath)
}

