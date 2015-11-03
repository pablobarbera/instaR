#' @rdname getFollowers
#' @export
#'
#' @title 
#' Get the list of users this user is followed by.
#'
#' @description
#' \code{getFollowers} retrieves the list of users that follow a given user,
#' as well as basic information about all of them.
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
#' @param verbose If \code{TRUE} (default), outputs details about progress
#' of function on the console.
#'
#'
#' @examples \dontrun{
#' ## See examples for instaOAuth to know how token was created.
#' ## Downloading list of followers of @@senjohnmccain
#'  load("my_oauth")
#'  mccain <- getFollowers( username="senjohnmccain", token=my_oauth )
#' }
#'


getFollowers <- function(username, token, userid=NULL, verbose=TRUE){

    if (is.null(userid)){
        url <- paste0("https://api.instagram.com/v1/users/search?q=", username)
        content <- callAPI(url, token)
        if (length(content$data)==0) stop("Error. User name not found.")
        userid <- as.numeric(content$data[[1]]$id)
    }

    url <- paste0("https://api.instagram.com/v1/users/", userid, 
        "/followed-by?count=100")
    content <- callAPI(url, token)
    l <- length(content$data)
    if (verbose) message(l, " followers")

    ## Error trap
    if (length(content$data)==0){ 
        stop("Error. Zero followers?")
    }

    df <- userListToDF(content$data)
    

    if (length(content$pagination)>0){

        df.list <- list(df)
        while (length(content$data)>0 && (length(content$pagination)!=0) &&
            !is.null(content$pagination['next_url'])){
            #message(content$pagination$'next_url')
            error <- tryCatch(content <- callAPI(content$pagination['next_url'], 
                token), error=function(e) e)
            if (inherits(error, 'error')){
                Sys.sleep(1)
                content <- callAPI(content$pagination['next_url'], token)
            }
            l <- l + length(content$data)
            if (length(content$data)>0){ message(l, " followers")}  
                   
            new.df <- userListToDF(content$data)
     
            df.list <- c(df.list, list(new.df))
        }
        df <- do.call(rbind, df.list)
    }
    
    return(df)
}


