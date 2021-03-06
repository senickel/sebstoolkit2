#' write_csv_version function
#'
#' check ordinal if they are ordinal
#' @param Click here and there
#' @keywords ordinal
#' @keywords
#' @export
#' @examples
#' @importFrom data.table fwrite

write_csv_version<-function(data,filepath,filename) {
  library(data.table)
  vc<-version_counter(filepath,filename,".csv")
  filepath<-paste0(filepath,filename,vc,".csv")
  fwrite(data,filepath)
}

