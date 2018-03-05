#' text_rem.special.letters
#'
#' check nominal if they are nominal
#' @param Click here and there
#' @keywords nominal
#' @keywords
#' @export
#' @examples 
#' checking.nominal()
#' @importFrom magrittr %>%
#' 

text_rem.special.letters<-function(x) {
  special<-c(",","/","(",")","?","!","'")
  sapply(special,function(x1) x<<-gsub(x1," ",x,fixed=TRUE)) %>% as.character
  for (i in 1:3) x<-gsub("  "," ",x)
  if (substr(x,nchar(x),nchar(x))==" ") x<-substr(x,1,nchar(x)-1) 
  
  return(x)
}
