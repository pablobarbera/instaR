\name{getUserMedia}
\alias{getUserMedia}
\title{Extract recent media published by a user.}
\usage{
  getUserMedia(username, token, n = 30, folder = NULL,
    userid = NULL, verbose = TRUE)
}
\arguments{
  \item{username}{String, screen name of user.}

  \item{token}{An OAuth token created with
  \code{instaOAuth}.}

  \item{n}{Maximum number of media to return.}

  \item{folder}{If different than \code{NULL}, will
  download all pictures to this folder.}

  \item{userid}{Numeric ID of user.}

  \item{verbose}{If \code{TRUE} (default), outputs details
  about progress of function on the console.}
}
\description{
  \code{getUserMedia} retrieves public media from a given
  user and, optionally, downloads recent pictures to a
  specified folder.
}
\examples{
\dontrun{
## See examples for instaOAuth to know how token was created.
## Capturing information about 50 most recent pictures by @barackobama
 load("my_oauth")
 obama <- getUserMedia( username="barackobama", token=my_oauth, n=50, folder="barackobama")
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}

