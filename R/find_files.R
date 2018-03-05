#' find_files
#'
#' 
#' @param Click here and there
#' @keywords nominal
#' @keywords
#' @export
#' @examples 
#' 
#' @importFrom magrittr %>%
#' 
find_files <- function(lookfor,p="C:/Users/",filetype="R",alltolower=TRUE,subdir=TRUE) {
  findit <- function(path,filetype,lookfor) {
    files<-list.files(path,pattern=paste("\\.",filetype,"$",sep=""))
    save<-sapply(files,function(f1) {
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


