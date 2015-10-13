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
drupalr.logout(h)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{
  The handle provided by the drupalr.authenticate function used to login.
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
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
function (h) 
{
    GET(paste0(h["url"], "/user/logout"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
