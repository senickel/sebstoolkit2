#' write_log
#'
#' @param Click here and there
#' @keywords nominal
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#' @importFrom pryr mem_used
#'
write_log<-function(string,filenameaddition="",filepath=NULL) {
  if (is.null(filepath)) stop("No path specified.")
  string<-paste0(Sys.time() %>%
                   format(.,"%Y/%m/%d %H:%M"),
                 "\t",
                 string,"\t[",round(mem_used()/10^9,4),"GB]\n")
  cat(string,
      file=paste0(filepath,filenameaddition,"_",Sys.time() %>% format(.,"%Y_%m_%d"),".txt"),
      append=TRUE,sep = "")

}
