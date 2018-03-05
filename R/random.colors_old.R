#' random.colors
#'
#' check nominal if they are nominal
#' @param Click here and there
#' @keywords nominal
#' @keywords
#' @export
#' @examples
#' @importFrom magrittr %>%
#'

random.colors_old<-function(nmbr,seed=1337,exclude_color="",exclude.vector=NULL) {
  number<-round(nmbr*2)
  set.seed(seed)
  clz<-sample(0:strtoi("0xFFFFFF"),number)

  hex<-sapply(as.hexmode(clz) %>% as.character,function(x) {
    if (nchar(x)<6) x<-paste0(rep(0,6-nchar(x)),x,collapse="")
    return(x)
  })
  # exclude all grey
  rgb<-data.frame(r=substr(hex,1,2),
                  g=substr(hex,3,4),
                  b=substr(hex,5,6))


  rgb<-rgb[!apply(rgb,1,function(x) x[1]==x[2]&x[2]==x[3]),]

  #exclude all red
  hex2number <- function(variables) strtoi(paste0("0x",variables))


  if (exclude_color=="red") {
    rgb<-rgb[!apply(rgb,1,function(x) {
      x<-sapply(x,hex2number)
      return(x[1]>x[2]&x[1]>x[3])
    }),]
  } else if (exclude_color=="blue") {
    rgb<-rgb[!apply(rgb,1,function(x) {
      x<-sapply(x,hex2number)
      return(x[3]>x[2]&x[3]>x[1])
    }),]
  } else if (exclude_color=="green") {
    rgb<-rgb[!apply(rgb,1,function(x) {
      x<-sapply(x,hex2number)
      return(x[2]>x[1]&x[2]>x[3])
    }),]
  }

  # make color
  hex<-apply(rgb,1,function(x) paste0(x,collapse="")) %>% unique %>% paste0("#",.)

  if (!is.null(exclude.vector)) hex<-hex[!hex%in%exclude.vector]

  while(hex %>% length<nmbr) {
    hex<-c(hex,random.colors(nmbr-length(hex),seed = seed+10)) %>% unique
  }

  return(hex[1:nmbr])
}
