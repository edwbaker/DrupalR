drupalr.authenticate <- function(d_url, d_name, d_pass) {
  pars=list(
    op="Log in",
    name = d_name,
    pass= d_pass,
    form_id="user_login"
  )
  curl = RCurl::getCurlHandle()
  curl = DrupalR::drupalr.agent("DrupalR", curl)
  RCurl::curlSetOpt(cookiejar="cookies.txt", followlocation = TRUE, curl=curl)
  html = RCurl::postForm(paste(paste0(d_url,"/user/login")), .params = pars, curl=curl)
  return(curl)
}

drupalr.logout <- function(d_url, c) {
  getURL(paste(d_url,"user/logout"), curl = c)
}

drupalr.get <- function(d_url, path, c = getCurlHandle()) {
  return(RCurl::getURL(paste0(d_url,path), curl = c))
}

drupalr.agent <- function(agent, c) {
  RCurl::curlSetOpt(useragent=agent, curl = c)
  return(c)
}

drupalr.postForm <- function(d_url, d_path, form_id, pars, c, num=1) {
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

drupalr.postComment <- function(d_url, d_path, body, params, curl) {
  comment_params = list(
    op="Save",
    'comment_body[und][0][value]' = body,
    'comment_body[und][0][format]' = 'comment_filtered_html'
  );
  params <- c(params, comment_params);
  DrupalR::drupalr.postForm(d_url, d_path, "comment_form", params, curl);
}
