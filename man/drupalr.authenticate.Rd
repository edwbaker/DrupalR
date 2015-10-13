\name{drupalr.authenticate}
\alias{drupalr.authenticate}

\title{
Authenticate to a Drupal website
}
\description{
This function authenticates to a Drupal website, returning a handle for future requests.
}
\usage{
drupalr.authenticate(d_url, d_name, d_pass)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d_url}{
  The base URL of the Drupal website you wish to authenticate to.
}
  \item{d_name}{
  Username on the Drupal website.
}
  \item{d_pass}{
  Password for the Drupal website.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A httr handle that can be used for future requests.
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
function (d_url, d_name, d_pass) 
{
    post <- POST(paste0(d_url, "/user/login"), body = list(name = d_name, 
        pass = d_pass, form_id = "user_login", op = "Log in"))
    h <- handle(d_url)
    return(h)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
