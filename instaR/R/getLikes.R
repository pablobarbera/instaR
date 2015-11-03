#' @rdname getLikes
#' @export
#'
#' @title 
#' Get the list of users who liked a photo.
#'
#' @description
#' \code{getLikes} retrieves the list of users who liked a photo.
#'
#' @author
#' Tiago Dantas \email{t.mendesdantas@@gmail.com}
#'
#' @param id Numeric ID of photo.
#' 
#' @param token An OAuth token created with \code{instaOAuth}.
#'
#'
#' @param verbose If \code{TRUE} (default), outputs details about progress
#' of function on the console.
#'
#'
#' @examples \dontrun{
#' ## See examples for instaOAuth to know how token was created.
#' ## Downloading list of users who liked @@barackobama's most recent photo
#'  load("my_oauth")
#'  obama <- getUserMedia( username="barackobama", token=my_oauth, n=1)
#'  likes <- getLikes(obama$id[1], token=my_oauth)
#' }
#'

getLikes <- function(id, token, verbose=TRUE){
  
  url <- paste0("https://api.instagram.com/v1/media/",id,"/likes")
  content <- callAPI(url, token)
  l <- length(content$data)
  if (verbose) message(l, " likes")
  
  if (length(content$data)==0){ 
    stop("Error. No Likes?")
  }
  df <- likesListToDF(content$data)
  return(df)
}
