\name{instaOAuth}
\alias{instaOAuth}
\title{Create OAuth token to Intagram R session}
\usage{
  instaOAuth(app_id, app_secret, scope = "basic")
}
\arguments{
  \item{app_id}{numeric, Client ID of application to be
  used to create OAUth token. Available at
  \url{http://instagram.com/developer}}

  \item{app_secret}{string, Client Secret of application to
  be used to create OAUth token. Available at
  \url{http://instagram.com/developer}.}
}
\description{
  \code{fbOAuth} creates an OAuth access token that enables
  R to make authenticated calls to the Instagram API. The
  token can be saved as a file in disk to be re-used in
  future sessions. This function relies on the \code{httr}
  package to create the OAuth token, and is a simplified
  version of one of its examples.
}
\details{
  To obtan your \code{app_id} and \code{app_secret}, do the
  following steps. First, go to
  \url{http://instagram.com/developer/} and register your
  application with any name. Then, run the
  \code{instaOAuth} function with your "Client ID" and
  "Client Secret" as arguments. It will return a URL, which
  you will need to paste into the field "OAuth
  redirect_uri" in your application settings on Instagram.
  After changing it, press Enter in R. A new browser window
  will open and sign the token. If everything works works
  well, you will get a message that says you can return to
  R.

  Note that the authentication process will NOT work
  correctly in RStudio. As an alternative, create your
  token in R from the command line interface or the R
  application, save it as a file, and then open it up from
  RStudio.
}
\examples{
\dontrun{
## an example of an authenticated request after creating the OAuth token
## where app_id and app_secret are fictitious, and token is saved for
## future sessions
 my_oauth <- instaOAuth(app_id="123456789", app_secret="1A2B3C4D")
 save(my_oauth, file="my_oauth")
 load("my_oauth")
 obama <- searchInstagram(tag="obama", token=my_oauth)
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{searchInstagram}}
}

