#' version_counter function
#'
#' check ordinal if they are ordinal
#' @param Click here and there
#' @keywords ordinal
#' @keywords
#' @export
#' @examples
#' get.1.sub(data,split,cols,whichcol,replace)
#'
#'
version_counter<-function(filepath,filename,filetype=".Rmd",increaseversion=1) {
  library(dplyr)
  fileversions<-list.files(filepath,pattern=glob2rx(paste0(filename,"*version*",filetype)),full.names = TRUE)
  if (fileversions %>% length==0) return("_version_1")
  paste0("_version_",fileversions %>% gsub(filepath,"",.) %>% gsub(filetype,"",.) %>% strsplit(.,"_version_") %>%
           get.element.from.list(.,2) %>% as.numeric %>% max +increaseversion)
}
