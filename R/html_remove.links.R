#' html_remove.links
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
html_remove.links<-function(x) {
  while(grepl("<a",x)) {
    as<-gregexpr("<a",x)[[1]][1]
    ae<-gregexpr("</a>",x)[[1]][1]
    ender<-gregexpr(">",x)[[1]]  
    as2<-ender[ender>as] %>% min
    ae2<-ender[ender>ae] %>% min
    if (as == 1) {
      x<-paste0(substr(x,as2+1,ae-1),substr(x,ae2+1,nchar(x)))
    } else if (ae2==nchar(x)) {
      x<-paste0(substr(x,1,as-1),substr(x,as2+1,ae-1))
    } else {
      x<-paste0(substr(x,1,as-1),substr(x,as2+1,ae-1),substr(x,ae2+1,nchar(x)))
    }
  }
  return(x)
}

