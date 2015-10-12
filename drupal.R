library(httr);

drupalr.authenticate <- function(d_url, d_name, d_pass) {
  post <- POST(paste0(d_url, "/user/login"), body = list(name = d_name, pass = d_pass, form_id = "user_login", op="Log in"));
  h <- handle(d_url);
  return (h);
}

drupalr.logout <- function(h) {
  GET(paste0(h['url'], "/user/logout"));
}

drupalr_get <- function(h, path) {
  get <- GET(paste0(h['url'], "/", path));
  return (get);
}