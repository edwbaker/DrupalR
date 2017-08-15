\name{drupalr.postComment}
\alias{drupalr.postComment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Post a comment to a Drupal site.
}
\description{
This function allows a comment to be posted to a Drupal node.
}
\usage{
drupalr.postComment(d_url, d_path, body, params, c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d_url}{
  The base URL of teh Drupal site.
  }
  \item{d_path}{
  The path to the node you are commenting on.
  }
  \item{body}{
  The comment body.
  }
  \item{params}{
  Additional CURL parameter for optional fields.
  }
  \item{c}{
  A CURL handler for the authenticated user.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
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
function(d_url, d_path, body, params, curl) {
  comment_params = list(
    op="Save",
    'comment_body[und][0][value]' = body,
    'comment_body[und][0][format]' = 'comment_filtered_html'
  );
  params <- c(params, comment_params);
  drupalr.postForm(d_url, d_path, "comment_form", params, curl);
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
