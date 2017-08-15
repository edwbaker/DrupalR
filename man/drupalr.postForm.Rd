\name{drupalr.postForm}
\alias{drupalr.postForm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to submit a Drupal form.
}
\description{
This function allows a Drupal form to be submitted (e.g. user login form, node submission form, etc).
}
\usage{
drupalr.postForm(d_url, d_path, form_id, pars, c, num=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d_url}{
  Base URL of the Drupal site.
  }
  \item{d_path}{
  Relative path to the Drupal page containing the form.
  }
  \item{form_id}{
  The Drupal form_id of the target form.
  }
  \item{pars}{
  CURL parameters for the fields int he form.
  }
  \item{c}{
  CURL handler of an authenticated session.
  }
  \item{num}{
  Offset of the form.
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
function(d_url, d_path, form_id, pars, c, num=1) {
  #First load form to get token and build_id
  form <- RCurl::getURL(paste0(d_url, d_path), curl = c);
  data <- xml2::read_html(form)
  
  if (form_id == "comment_form") { num <- 3}
  
  data <- xml2::xml_find_first(data, paste0('//*[(@id = "',gsub("_", "-", form_id), '")]'));
  form_token <- unname(xml2::xml_attrs(xml2::xml_find_all(data, '//*[(@name = "form_token")]'))[[num]]['value'])
  form_build_id <- unname(xml2::xml_attrs(xml2::xml_find_all(data, '//*[(@name = "form_build_id")]'))[[num]]['value'])
  form_id <- unname(xml2::xml_attrs(xml2::xml_find_all(data, '//*[(@name = "form_id")]'))[[num]]['value'])
  
  i_pars=list(
    form_id=form_id,
    form_token=form_token,
    form_build_id=form_build_id
  );
  
  pars = c(pars, i_pars);
  
  html = RCurl::postForm(paste0(d_url, d_path), .params = pars, curl=c)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
