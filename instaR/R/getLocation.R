#' @rdname getLocation
#' @export
#'
#' @title 
#' Get basic information about a location using a location ID
#'
#' @description
#' \code{getLocation} retrieves location information
#'
#' @author
#' Jonne Guyt \email{j.y.guyt@@uva.nl}
#'
#' @param location id.
#' 
#' @param token An OAuth token created with \code{instaOAuth}.
#'
#' @examples \dontrun{
#' ## See examples for instaOAuth to know how token was created.
#' ## Capturing information about a location
#'  load("my_oauth")
#'  loc_id_info <- getLocation( location=locid, token=my_oauth,)
#' }
#'

getLocation <- function(locid, token){
  
  url <- paste0("https://api.instagram.com/v1/locations/", locid)
  content <- callAPI(url, token)
  if (content$meta$code==400){
    stop(content$meta$error_message)
  }
  
  if (length(content$data)==0){ 
    stop("Location ID not known")
  }
  data <- content$data
  data[sapply(data, is.null)] <- NA
  df <- data.frame(data$latitude, data$longitude, data$id, data$name,
                   stringsAsFactors=F)
  return(df)
}

