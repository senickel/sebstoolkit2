#' change.first.letters
#'
#' Changes the first letter of each word to capital letter except for help-words such as
#' @param Click here and there
#' @keywords nominal
#' @keywords
#' @export
#' @examples 
#' checking.nominal()
#' @importFrom magrittr %>%
#' 

change.first.letters<-function(vector) {
  sapply(vector,function(strg) {
  strg<-strsplit(strg," ")  %>% unlist
  prep<-c("by","for","a","an","and","to","the","if","in","at","of","off","or","with","after","on","etc")
  whch<-tolower(strg)%>%   gsub("[[:punct:]]","",.)%in%prep
  whch[1]<-FALSE #First one always capital
  strg<-sapply(strg,function(x1) paste(toupper(substr(x1,1,1)),substr(x1,2,nchar(x1)),sep=""))
  strg[whch]<-sapply(strg[whch],tolower)
  return(paste(strg,collapse=" "))
  })
}
