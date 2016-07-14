#' @rdname searchInstagram
#' @export
#'
#' @title 
#' Search public media that mention a specific hashtag, or that were sent
#' from a given area
#'
#' @description
#' \code{searchInstagram} retrieves public pictures and video whose caption 
#' mentions a given hashtag, or that were sent within a given area, delimited
#' by a set of coordinates and a radius. It returns a data frame with
#' information about each picture or video, and optionally will download
#' all pictures to a specific folder
#'
#' @details
#'
#' It is only possible to apply one filter at a time: either search by hashtag
#' OR search by coordinates.
#'
#' The \code{mindate} and \code{maxdata} search parameters only work when searching
#' by location, not when searching by tag.
#'
#' IMPORTANT: After June 1st, 2016 only applications that have passed permission
#' review by Instagram will be allowed to access this API endpoint. See 
#' \url{https://www.instagram.com/developer/review/} for more information.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{instaOAuth}}
#'
#' @param tag Hashtag used to filter media. It is only possible for a single
#' hashtag.
#' 
#' @param token An OAuth token created with \code{instaOAuth}.
#'
#' @param n Maximum number of media to return.
#'
#' @param lat Latitude of the center search coordinate
#'
#' @param lng Longitude of the center search coordinate
#' 
#' @param distance Default is 1km (distance=1000), max distance is 5km.
#' 
#' @param mindate Minimum date for search period
#' 
#' @param maxdate Maximum date for search period
#'
#' @param folder If different than \code{NULL}, will download all pictures
#' to this folder.
#'
#' @param verbose If \code{TRUE} (default), outputs details about progress
#' of function on the console.
#'
#' @param sleep Number of seconds between API calls (default is 0).
#'
#' @examples \dontrun{
#' ## See examples for instaOAuth to know how token was created.
#' ## Searching and downloading 100 public media that mention #obama
#'  load("my_oauth")
#'  obama <- searchInstagram( tag="obama", token=my_oauth, n=100, folder="obama")
#' ## Searching and downloading pictures sent from Times Square with a minimum date 
#' ## of 2013-12-31 and a maximum date of 2014-01-01
#'  tsq <- searchInstagram( lat=40.7577, lng=-73.9857, distance=500, 
#'     token=my_oauth, n=500, folder="timessquare", 
#'     mindate="2014-12-31", maxdate="2014-01-01")
#' }
#'

searchInstagram <- function(tag=NULL, token, n=100, lat=NULL, lng=NULL, 
                            distance=NULL, folder=NULL, mindate=NULL, maxdate=NULL, verbose=TRUE, sleep=0){
  
  if (!is.null(tag)) url <- paste0("https://api.instagram.com/v1/tags/", tag, "/media/recent?")
  if (!is.null(lat) && !is.null(lng)) {
    url <- paste0("https://api.instagram.com/v1/media/search?lat=", lat, 
                  "&lng=", lng)
    if (!is.null(distance)) url <- paste0(url, "&distance=", distance)
    url <- paste0(url, "&count=", min(c(n, 100)))
  }
  if (!is.null(mindate)) url <- paste0(url,"&min_timestamp=",as.numeric(as.POSIXct(mindate)))
  if (!is.null(maxdate)) url <- paste0(url,"&max_timestamp=",as.numeric(as.POSIXct(maxdate)))
  if (!is.null(tag) & !is.null(mindate)){
    message('"mindate" and "maxdate" options only work in combination with a location and will be ignored.')
  }
  if (!is.null(mindate) && as.POSIXct(mindate) > as.POSIXct(maxdate)){
    stop('"mindate" should be less than "maxdate".')
  }
  content <- callAPI(url, token)
  l <- length(content$data)
  if (l==0) stop(c("0 posts found. Does this application have permission to access public content?",
      " See ?searchInstagram for more information"))
  if (verbose) message(l, " posts")
  
  ## retrying 3 times if error was found
  error <- 0
  while (is.null(content$meta) | content$meta != 200){
    message("Error!"); Sys.sleep(0.5); error <- error + 1
    content <- callAPI(url, token)      
    if (error==3){ stop("Error") }
  }
  if (length(content$data)==0){ 
    stop("No public posts mentioning the string were found")
  }
  
  df <- searchListToDF(content$data)
  
  
  if (!is.null(folder)){
    if (verbose) message("Downloading pictures...")
    # creating folder if it doesn't exist
    dir.create(file.path(getwd(), folder), showWarnings = FALSE)
    downloadPictures(df, folder)
  }
  
  if (sleep!=0){ Sys.sleep(sleep)}
  
  if (n>20){
    
    df.list <- list(df)
    
    if (length(content$pagination)>0) next_url <- content$pagination['next_url']
    if (length(content$pagination)==0 & is.null(mindate)){
      next_url <- paste0(url, "&max_timestamp=", 
                         as.numeric(min(df$created_time)))
    }
    if (length(content$pagination)==0 & !is.null(mindate)){
      next_url <- gsub('max_timestamp=([0-9]{10})', 
        paste0('max_timestamp=',as.numeric(min(df$created_time))), url)
    }    

    while (l<n & length(content$data)>0 & !is.null(next_url[[1]])){
      
      content <- callAPI(next_url, token)
     
      ## retrying 3 times if error was found
      error <- 0
      while (is.null(content$meta) | content$meta != 200){
        message("Error!"); Sys.sleep(0.5); error <- error + 1
        content <- callAPI(url, token)      
        if (error==3){ stop("Error") }
      }
      
      new.df <- searchListToDF(content$data)
      if (all(new.df$id %in% unlist(lapply(df.list, '[[', 'id'))== TRUE)){
        break
      }

      l <- l + length(content$data)
      if (length(content$data)>0){ message(l, " posts")}  
        
      # downloading pictures
      if (!is.null(folder)){
        if (verbose) message("Downloading pictures...")
        downloadPictures(new.df, folder)
      }
        
      df.list <- c(df.list, list(new.df))
        
      if (length(content$pagination)>0) next_url <- content$pagination['next_url']
      if (length(content$pagination)==0 & is.null(mindate)){
        next_url <- paste0(url, "&max_timestamp=", 
                         as.numeric(min(new.df$created_time)))
      }
      if (length(content$pagination)==0 & !is.null(mindate)){
        next_url <- gsub('max_timestamp=([0-9]{10})', 
          paste0('max_timestamp=',as.numeric(min(new.df$created_time))), url)
      } 
      if (sleep!=0){ Sys.sleep(sleep)}
    }
    df <- do.call(rbind, df.list)
  }
  
  return(df)
}

