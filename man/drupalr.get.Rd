\name{drupalr.get}
\alias{drupalr.get}
\title{
GET a page from a Drupal site
}
\description{
Used to fetch a page froma  Drupal website using a HTTP GET request.
}
\usage{
drupalr.get(h, path, userAgent = NULL)
}
\arguments{
  \item{h}{
  A handle from a previous authentication.
}
  \item{path}{
  The path on the site you wish to access (no preceding /).
}
  \item{userAgent}{
  Optionally specify a custom user-agent string.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A httr response.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Ed Baker
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (h, path, userAgent = NULL) 
{
    if (is.null(userAgent)) {
        userAgent <- "DrupalR"
    }
    get <- GET(paste0(h["url"], "/", path), user_agent = userAgent)
    return(get)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
