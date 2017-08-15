\name{drupalr.agent}
\alias{drupalr.agent}

\title{
DrupalR User-agent
}
\description{
This function allows you to set the user-agent string used by DrupalR.
}
\usage{
drupalr.agent(agent, c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{agent}{
  The user-agent string you wish to send.
}
  \item{c}{
  A valid CURL handler from drupalr.authenticate.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A modified CURL object with the specified user-agent.
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
drupalr.agent <- function(agent, c) {
  curlSetOpt(useragent=agent, curl = c)
  return(c)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
