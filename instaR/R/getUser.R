#' @rdname getUser
#' @export
#'
#' @title 
#' Get basic information about a user
#'
#' @description
#' \code{getUser} retrieves public information about Instagram user
#'
#' @details
#'
#' IMPORTANT: After June 1st, 2016 only applications that have passed permission
#' review by Instagram will be allowed to access data for users other than the
#' authenticated user. See  \url{https://www.instagram.com/developer/review/} 
#' for more information.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param username String, screen name of user.
#' 
#' @param token An OAuth token created with \code{instaOAuth}.
#'
#' @param userid Numeric ID of user.
#'
#' @examples \dontrun{
#' ## See examples for instaOAuth to know how token was created.
#' ## Capturing information about @@barackobama
#'  load("my_oauth")
#'  obama <- getUser( username="barackobama", token=my_oauth,)
#' }
#'

getUser <- function(username, token, userid=NULL){

    if (is.null(userid)){
        url <- paste0("https://api.instagram.com/v1/users/search?q=", username)
        content <- callAPI(url, token)
        if (length(content$data)==0) stop(c("Error. User name not found. ",
            "Does this application have permission to access public content?"))
        userid <- as.numeric(content$data[[1]]$id)
    }

    url <- paste0("https://api.instagram.com/v1/users/", userid)
    content <- callAPI(url, token)
    if (content$meta$code==400){
        stop(content$meta$error_message)
    }

    if (length(content$data)==0){ 
        stop("No public data about user was found")
    }

    df <- userDataToDF(content$data)
    return(df)
}


