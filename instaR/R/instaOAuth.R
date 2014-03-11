#' @rdname fbOAuth
#' @export
#'
#' @title 
#' Create OAuth token to Intagram R session
#'
#' @description
#' \code{fbOAuth} creates an OAuth access token that enables R to make
#' authenticated calls to the Instagram API. The token can be saved as a
#' file in disk to be re-used in future sessions. This function relies on the
#' \code{httr} package to create the OAuth token, and is a simplified version
#' of one of its examples.
#'
#' @details
#' To obtan your \code{app_id} and \code{app_secret}, do the following steps.
#' First, go to \url{http://instagram.com/developer/} and register your
#' application with any name. Then, run the \code{instaOAuth} function with
#' your "Client ID" and "Client Secret" as arguments. It will return a URL,
#' which you will need to paste into the field "OAuth redirect_uri" in
#' your application settings on Instagram. After changing it, press Enter in
#' R. A new browser window will open and sign the token. If everything works
#' works well, you will get a message that says you can return to R.
#'
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{searchInstagram}}
#'
#' @param app_id numeric, Client ID of application to be used to create OAUth token. Available
#' at \url{http://instagram.com/developer}
#' 
#' @param app_secret string, Client Secret of application to be used to create OAUth token.
#' Available at \url{http://instagram.com/developer}.
#'
#'
#' @examples \dontrun{
#' ## an example of an authenticated request after creating the OAuth token
#' ## where app_id and app_secret are fictitious, and token is saved for
#' ## future sessions
#'  my_oauth <- instaOAuth(app_id="123456789", app_secret="1A2B3C4D")
#'  save(my_oauth, file="my_oauth")
#'  load("my_oauth")
#'  obama <- searchInstagram(tag="obama", token=my_oauth)
#' }
#'

instaOAuth <- function(app_id, app_secret, scope="basic"){

    ## getting callback URL
    full_url <- oauth_callback()
    full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")

    message <- paste("Copy and paste into 'OAuth redirect_uri' on Instagram App Settings:", 
        full_url, "\nWhen done, press any key to continue...")
    ## prompting user to introduce callback URL in app page
    invisible(readline(message))
    ## using httr package functions
    instagram <- oauth_endpoint(NULL, "authorize", "access_token",
        base_url = "https://api.instagram.com/oauth")
    myapp <- oauth_app("instagram", app_id, app_secret)

    ## before httr 0.3
    if (packageVersion('httr')$minor < 3){
        insta_token <- oauth2.0_token(instagram, myapp, scope=scope)
        token <- sign_oauth2.0(insta_token$access_token)
        if (GET("https://api.instagram.com/v1/users/self/feed?count=1", 
            config=token)$status==200){
            message("Authentication successful.")
        }
    }

    ## with httr 0.3
    if (packageVersion('httr')$minor >= 3){
        token <- oauth2.0_token(instagram, myapp, cache=FALSE, scope=scope)
        if (GET(paste0("https://api.instagram.com/v1/users/self/feed?count=1", 
                "&access_token=", 
                token$credentials$access_token))$status==200){
            message("Authentication successful.")
        }   
    }

    return(token)
}










