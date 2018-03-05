#' version_counter function
#'
#' check ordinal if they are ordinal
#' @param filepath path to file
#' @param filename name to file
#' @param filetype type of fileending. Default is ".Rmd"
#' @param increaseversion by which number should the version counter be increased? Default is 1.
#' @keywords
#' @export
#' @examples
#'
version_counter<-function(filepath,filename,filetype=".Rmd",increaseversion=1) {
  library(dplyr)
  fileversions<-list.files(filepath,pattern=glob2rx(paste0(filename,"*version*",filetype)),full.names = TRUE)
  if (fileversions %>% length==0) return("_version_1")
  paste0("_version_",fileversions %>% gsub(filepath,"",.) %>% gsub(filetype,"",.) %>% strsplit(.,"_version_") %>%
           get.element.from.list(.,2) %>% as.numeric %>% max +increaseversion)
}
