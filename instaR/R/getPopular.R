#' @rdname getPopular
#' @export
#'
#' @title 
#' Returns 24 popular instagram posts information
#'
#' @description
#' \code{getPopular} retrieves up to 24 popular instagram posts
#'
#' @details
#' IMPORTANT: After June 1st, 2016 only applications that have passed permission
#' review by Instagram will be allowed to access data for users other than the
#' authenticated user. See  \url{https://www.instagram.com/developer/review/} 
#' for more information.
#'
#' @author
#' Jonne Guyt \email{j.y.guyt@@uva.nl}
#'
#' @param token An OAuth token created with \code{instaOAuth}.
#'
#' @param verbose If \code{TRUE} (default), outputs details about progress
#' of function on the console.
#'
#' @examples \dontrun{
#' ## See examples for instaOAuth to know how token was created.
#' ## Downloading list of popular instagram posts
#'  load("my_oauth")
#'  popular_posts <- getPopular(token=my_oauth )
#' }
#'


getPopular <- function(token, verbose=TRUE){
  
  url <- paste0("https://api.instagram.com/v1/media/popular?access_token=ACCESS-TOKEN")
  content <- callAPI(url, token)
  
  l <- length(content$data)
  if (verbose) message(l, " popular videos/pics")
  
  ## Error trap
  if (length(content$data)==0){ 
    stop("Error. Nothing popular?")
  }
  
  df <- popularDataToDF(content$data)
  return(df)
}

