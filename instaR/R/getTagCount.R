#' @rdname getTagCount
#' @export
#'
#' @title 
#' Get number of times a tag has been used
#'
#' @description
#' \code{getTagCount} retrieves a count of the number of times a tag has
#' been used in comments to a picture or video.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param tag Hashtag used to filter media.
#' 
#' @param token An OAuth token created with \code{instaOAuth}.
#'
#'
#' @examples \dontrun{
#' ## See examples for instaOAuth to know how token was created.
#' ## Capturing information about #nofilter
#'  load("my_oauth")
#'  getTagCount( tag="nofilter, token=my_oauth,)
#' }
#'

getTagCount <- function(tag, token){

    url <- paste0("https://api.instagram.com/v1/tags/", tag)
    content <- callAPI(url, token)
    if (content$meta$code==400){
        stop(content$meta$error_message)
    }
    count <- content$data$media_count
    return(count)
    
}


