\name{drupalr.logout}
\alias{drupalr.logout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Logout of a Drupal site
}
\description{
Used to logout of a Drupal website at the end of a session, or to change users.
}
\usage{
drupalr.logout(d_url, c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d_url}{
  The base URL of the Drupal site.
  }
  \item{c}{
  A CURL object containing the session to be logged out.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
  Ed Baker
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(d_url, c) {
  getURL(paste(d_url,"user/logout"), curl = c)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
