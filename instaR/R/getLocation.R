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
#' @param location_id numeric, location id.
#' 
#' @param token An OAuth token created with \code{instaOAuth}.
#'
#' @examples \dontrun{
#' ## See examples for instaOAuth to know how token was created.
#' ## Capturing information about a location
#'  load("my_oauth")
#'  loc_id_info <- getLocation( location_id=423423, token=my_oauth,)
#' }
#'

getLocation <- function(location_id, token){
  
  url <- paste0("https://api.instagram.com/v1/locations/", location_id)
  content <- callAPI(url, token)
  if (content$meta$code==400){
    stop(content$meta$error_message)
  }
  
  if (length(content$data)==0){ 
    stop("Location ID not known")
  }
  data <- content$data
  data[sapply(data, is.null)] <- NA
  df <- data.frame(latitude=data$latitude, longitude=data$longitude, 
                    location_id=data$id, location_name=data$name,
                   stringsAsFactors=F)
  return(df)
}

