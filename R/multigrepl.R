#' multigrepl
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

multigrepl <- function(vector,greplvector,...) {
  sapply(greplvector,function(g) {
    return(grepl(g,vector,...))
  }) %>% apply(.,1,sum) %>% sapply(.,as.logical)
}
