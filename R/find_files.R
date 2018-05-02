#' find_files
#'
#'
#' @param lookfor string to be found
#' @param p Path where the search takes place. The more precise, the shorter the time to find. Default is "C:/Users/"
#' @param filetype type of file. Default is "R"
#' @param alltolower boolean. casesenstivity. default is TRUE
#' @param subdir should the search be extended to subdirectories. Default is TRUE
#' @export
#' @examples
#'
#' @importFrom magrittr %>%
#'
find_files <- function(lookfor,p="C:/Users/",filetype="R",alltolower=TRUE,subdir=TRUE) {
  findit <- function(path,filetype,lookfor) {
    files<-list.files(path,pattern=paste("\\.",filetype,"$",sep=""))
    save<-sapply(files,function(f1) {
      if (file.access(paste(path,f1,sep="/"),mode=4)==-1) return(-1)
      rl<-readLines(paste(path,f1,sep="/"),warn = FALSE)
      if (alltolower) rl<-rl %>% tolower
      c1<-gregexpr(lookfor,rl) %>% unlist
      if (c1[c1!=-1] %>% length > 0) return(paste(path,f1,sep="/"))
    })
    return(save)
  }

  if (subdir) {
    sbdr<-list.dirs(p)
    g<-lapply(sbdr,function(sb) findit(sb,filetype,lookfor)) %>% unlist
  } else {
    g<-findit(p,filetype,lookfor)
  }
  return(g[g!=-1] %>% as.character)
}
#


