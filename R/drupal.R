library(RCurl);

drupalr.authenticate <- function(d_url, d_name, d_pass) {
  pars=list(
    op="Log in",
    name = d_name,
    pass= d_pass,
    form_id="user_login"
  )
  curl = getCurlHandle()
  curl = drupalr.agent("DrupalR", curl)
  curlSetOpt(cookiejar="cookies.txt", followlocation = TRUE, curl=curl)
  html = postForm(paste(paste0(d_url,"/user/login")), .params = pars, curl=curl)
  return(curl)
}

drupalr.logout <- function(d_url, c) {
  getURL(paste(d_url,"user/logout"), curl = c)
}

drupalr.get <- function(d_url, path, c = getCurlHandle()) {
  return(getURL(paste0(d_url,path), curl = c))
}

drupalr.agent <- function(agent, c) {
  curlSetOpt(useragent=agent, curl = c)
  return(c)
}

drupalr.authenticate("bio.acousti.ca", "soundscape_bot", "password") -> c;

drupalr.postForm <- function(d_url, d_path, form_id, pars, c) {
  #First load form to get token and build_id
  form <- getURL(paste0(d_url, d_path), curl = c);
  data <- read_html(form)
  
  #TODO: Need to automatically identify correct form using form_id
  form_token <- unname(xml_attrs(xml_find_all(data, '//*[(@name = "form_token")]'))[[3]]['value'])
  form_build_id <- unname(xml_attrs(xml_find_all(data, '//*[(@name = "form_build_id")]'))[[3]]['value'])
  
  i_pars=list(
    form_id=form_id,
    form_token=form_token,
    form_build_id=form_build_id
  );
  
  pars = c(pars, i_pars);
  
  html = postForm(paste(paste0("bio.acousti.ca","/content/neoxabea-bipunctata-calling-sassafras")), .params = pars, curl=c)
}

drupalr.postComment <- function(d_url, d_path, body, params, curl) {
  comment_params = list(
    op="Save",
    'comment_body[und][0][value]' = body,
    'comment_body[und][0][format]' = 'comment_filtered_html'
  );
  params <- c(params, comment_params);
  drupalr.postForm(d_url, d_path, "comment_node_recording_form", params, curl);
}
