#' get.grep.vector
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

get.grep.vector<-function(vec,x) vec[grep(x,vec)]
