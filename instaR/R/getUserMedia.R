#' @rdname getUserMedia
#' @export
#'
#' @title 
#' Extract recent media published by a user.
#'
#' @description
#' \code{getUserMedia} retrieves public media from a given user and, optionally,
#' downloads recent pictures to a specified folder.
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
#' @param n Maximum number of media to return. Currently it is only possible to
#' download the 20 most recent pictures or videos on the authenticated user's 
#' profile, unless Instagram has approved the application.
#'
#' @param folder If different than \code{NULL}, will download all pictures
#' to this folder.
#'
#' @param userid Numeric ID of user.
#'
#' @param verbose If \code{TRUE} (default), outputs details about progress
#' of function on the console.
#'
#'
#' @examples \dontrun{
#' ## See examples for instaOAuth to know how token was created.
#' ## Capturing information about 50 most recent pictures by @@barackobama
#'  load("my_oauth")
#'  obama <- getUserMedia( username="barackobama", token=my_oauth, n=50, folder="barackobama")
#' }
#'

getUserMedia <- function(username, token, n=30, folder=NULL, userid=NULL, verbose=TRUE){

    if (is.null(userid)){
        url <- paste0("https://api.instagram.com/v1/users/search?q=", username)
        content <- callAPI(url, token)
        if (length(content$data)==0) stop(c("Error. User name not found. ",
            "Does this application have permission to access public content?"))
        userid <- as.numeric(content$data[[1]]$id)
    }

    url <- paste0("https://api.instagram.com/v1/users/", userid, "/media/recent?count=",
        as.character(min(c(n, 30))))
    content <- callAPI(url, token)
    if (content$meta$code==400){
        stop(content$meta$error_message)
    }
    l <- length(content$data)
    if (verbose) message(l, " posts")

    ## retrying 3 times if error was found
    error <- 0
    while (is.null(content$meta) | content$meta != 200){
        message("Error!")
        Sys.sleep(0.5)
        error <- error + 1
        content <- callAPI(url, token)      
        if (error==3){ stop("Error") }
    }
    if (length(content$data)==0){ 
        stop("No public posts mentioning the string were found")
    }

    df <- searchListToDF(content$data)

    if (!is.null(folder)){
        if (verbose) message("Downloading pictures...")
        downloadPictures(df, folder)
    }

    if (n>20){

        df.list <- list(df)
        while (l<n & length(content$data)>0 && (length(content$pagination)!=0) &&
            !is.null(content$pagination['next_url'])){
                
            content <- callAPI(content$pagination['next_url'], token)
            l <- l + length(content$data)
            if (length(content$data)>0){ message(l, " posts")}  
        
            ## retrying 3 times if error was found
            error <- 0
            while (is.null(content$meta) | content$meta != 200){
                message("Error!")
                Sys.sleep(0.5)
                error <- error + 1
                content <- callAPI(url, token)      
                if (error==3){ stop("Error") }
            }
            
            new.df <- searchListToDF(content$data)
            
            # downloading pictures
            if (!is.null(folder)){
                if (verbose) message("Downloading pictures...")
                downloadPictures(new.df, folder)
            }
            
            df.list <- c(df.list, list(new.df))
        }
        df <- do.call(rbind, df.list)
    }
    
    return(df)
}


