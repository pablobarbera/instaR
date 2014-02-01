\name{searchInstagram}
\alias{searchInstagram}
\title{Search public media that mention a specific hashtag, or that were sent
from a given area}
\usage{
  searchInstagram(tag = NULL, token, n = 100, lat = NULL,
    lng = NULL, distance = NULL, folder = NULL,
    verbose = TRUE)
}
\arguments{
  \item{tag}{Hashtag used to filter media. It is only
  possible for a single hashtag.}

  \item{token}{An OAuth token created with
  \code{instaOAuth}.}

  \item{n}{Maximum number of media to return.}

  \item{lat}{Latitude of the center search coordinate}

  \item{lng}{Longitude of the center search coordinate}

  \item{distance}{Default is 1km (distance=1000), max
  distance is 5km.}

  \item{folder}{If different than \code{NULL}, will
  download all pictures to this folder.}

  \item{verbose}{If \code{TRUE} (default), outputs details
  about progress of function on the console.}
}
\description{
  \code{searchInstagram} retrieves public pictures and
  video whose caption mentions a given hashtag, or that
  were sent within a given area, delimited by a set of
  coordinates and a radius. It returns a data frame with
  information about each picture or video, and optionally
  will download all pictures to a specific folder
}
\details{
  This function will only return media up to 7 days old.

  It is only possible to apply one filter at a time: either
  search by hashtag OR search by coordinates.
}
\examples{
\dontrun{
## See examples for instaOAuth to know how token was created.
## Searching and downloading 100 public media that mention #obama
 load("my_oauth")
 obama <- searchInstagram( tag="obama", token=my_oauth, n=100, folder="obama")
## Searching and downloading pictures sent from Times Square
 tsq <- searchInstagram( lat=40.7577, lng=-73.9857, distance=500,
    token=my_oauth, n=500, folder="timessquare")
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{fbOAuth}}
}

