library(RCurl);

drupalr.authenticate <- function(d_url, d_name, d_pass) {
  pars=list(
    op="Log in",
    name = d_name,
    pass= d_pass,
    form_id="user_login"
  )
  curl = getCurlHandle()
  curlSetOpt(cookiejar="cookies.txt",  useragent="DrupalR", followlocation = TRUE, curl=curl)
  html = postForm(paste(paste0(d_url,"/user/login")), .params = pars, curl=curl)
  return(curl)
}

drupalr.logout <- function(d_url, c) {
  getURL(paste(d_url,"user/logout"), curl = c)
}

drupalr.get <- function(d_url, path, c) {
  return(getURL(paste0(d_url,path), curl = c))
}