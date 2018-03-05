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
random.colors<-function(nmbr,seed=1337,exclude_color="",exclude.vector=NULL,distinctiveness=60,removetoobright=TRUE) {
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

  if (removetoobright) rgb<-rgb[apply(rgb,1,function(x) sapply(x,function(y) strtoi(paste0("0x",y))) %>% sum)<700,]

  if (exclude_color!="") exclude_color<-col2rgb(exclude_color)

  # remove grey
  rgb<-rgb[!apply(rgb,1,function(x) x[1]==x[2]&x[2]==x[3]),]

  #exclude all red
  hex2number <- function(variables) strtoi(paste0("0x",variables))
  #exclude_number<-hex2number(exclude_color)

  if (exclude_color!="") {
    rgb<-rgb[apply(rgb,1,function(x) {
    x<-sapply(x,hex2number)
    return(abs(x[1]-exclude_color[1])>distinctiveness|
    abs(x[2]-exclude_color[2])>distinctiveness|
    abs(x[3]-exclude_color[2])>distinctiveness)
  }),]
  }


  # make color
  hex<-apply(rgb,1,function(x) paste0(x,collapse="")) %>% unique %>% paste0("#",.)

  if (!is.null(exclude.vector)) hex<-hex[!hex%in%exclude.vector]

  # remove if to bright
  while(hex %>% length<nmbr) {
    hex<-c(hex,random.colors(nmbr-length(hex),seed = seed+10)) %>% unique
  }

  return(hex[1:nmbr])
}
