#' repath
#'
#'
#' Changes backslashes in string in clipboard when run. Useful for windows paths.
#'
#' @keywords
#' @note Code heavily inspired by https://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile/12703931#12703931.
#' @export
#' @examples
#'
#'
repath <- function() {
  xa <- gsub('\\\\', '/', readClipboard())
  writeClipboard(paste(xa, collapse=" "))
}
