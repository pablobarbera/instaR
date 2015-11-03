#' @rdname getComments
#' @export
#'
#' @title 
#' Returns up to 150 comments on an instagram post
#'
#' @description
#' \code{getComments} retrieves up to 150 recent comments on an instagram post, 
#' including text, author and date.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param id String, id of instagram post.
#'
#' @param token An OAuth token created with \code{instaOAuth}.
#'
#' @param verbose If \code{TRUE} (default), outputs details about progress
#' of function on the console.
#'
#' @examples \dontrun{
#' ## See examples for instaOAuth to know how token was created.
#' ## Downloading list of followers of @@senjohnmccain
#'  load("my_oauth")
#'  obama <- getUserMedia( username="barackobama", token=my_oauth )
#'  comments <- getComments( obama$id[1], token=my_oauth )
#' }
#'


getComments <- function(id, token, verbose=TRUE){

    url <- paste0("https://api.instagram.com/v1/media/", id, 
        "/comments")
    content <- callAPI(url, token)
    l <- length(content$data)
    if (verbose) message(l, " comments")

    ## Error trap
    if (length(content$data)==0){ 
        stop("Error. No comments?")
    }

    df <- commentsListToDF(content$data)
    return(df)
}


