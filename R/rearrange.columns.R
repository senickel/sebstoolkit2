#' rearrange.columns
#'
#' Rearrange position of variables
#' @param Click here and there
#' @keywords nominal
#' @keywords
#' @export
#' @examples 
#' checking.nominal()
#' @importFrom magrittr %>%
#' 
rearrange.columns<-function(df,pos,var) {
  if (pos>ncol(df)) warning("too long!")
  if (pos==1) return(data.frame(df[,var],df[,-which(colnames(df)==var)]))
  if (pos==ncol(df)) return(df[,-which(colnames(df)==var)],data.frame(df[,var]))
  df1<-df[,1:(pos-1)]
  if (var%in%colnames(df1)) df1<-df1[,-which(colnames(df1)==var)]
  df2<-df[,pos:ncol(df)]
  if (var%in%colnames(df2)) df2<-df2[,-which(colnames(df2)==var)]
  df3<-data.frame(df1,df[,var],df2)
  colnames(df3)[pos]<-var
  return(df3)
}

