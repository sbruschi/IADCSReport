#library(RCurl)

.onLoad <- function(libname, pkgname) {
  #assign("pkg_globals", new.env(), envir=parent.env(environment()))
  #assign("R_IADCS_autorize_autorization", FALSE, pkg_globals)
}

R_IADCS_autorize_server <-function(session=NULL){  
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  token_api_url <- paste("https://jia.iadcs.org/cgi-bin/shiny_service.pl?token=",token, sep="")
  referer <- paste("https://", session$clientData$hostname, sep="")
  data <- getURL(token_api_url, ssl.verifypeer = FALSE, 
                 REFERER= session$clientData$url_hostname, 
                 httpheader = c('Shiny_report' = session$clientData$url_pathname))
  data_json<- fromJSON(data)
  
  print (data_json$valid_user)
  
  #browser()
  
  if (data_json$valid_user == TRUE){
      #assign("R_IADCS_autorize_autorization", TRUE,  pkg_globals)
  }else{
      #assign("R_IADCS_autorize_autorization", FALSE, pkg_globals)
  }
  return (data_json)
}

R_IADCS_getData_csv <- function(url, userpwd=NA, path= "./") {  
  data <- R_IADCS_getData(url, userpwd)
  # get data
  data_csv <- read.csv(textConnection(data))
  return(data_csv)
}

R_IADCS_getData_json <- function(url, userpwd=NA, path= "./") {
  data <- R_IADCS_getData(url, userpwd)
  # get data
  data_json <- fromJSON(data)
  return(data_json)
}


R_IADCS_getData <- function(url, userpwd=NA, path= "./") {
  
  # check token
  #browser()
  #R_IADCS_autorize_autorization = get("R_IADCS_autorize_autorization", pkg_globals)
  
  print (url)
  
  #if (R_IADCS_autorize_autorization == FALSE){
    #stop("No Autorize request. Use R_IADCS_autorize_server to setup the token")
  #}
  
  print (url)
  
  # basic auth username:password
  userpass <- R_IADCS_PortalAuth(path)
  if(is.na(userpwd) == FALSE){
    userpass <- userpwd
  }
  
  # cookies
  account <- unlist(strsplit(userpass, ":"))
  username <- account[1]
  password <- account[2]
    
  c_file <- tempfile()
  # create cookie file 
  if ( file.exists(c_file) == FALSE ) {
    file.create(c_file)
  }
  
  login_page<-sub('(iadcs.org\\b)(.)*', 'iadcs.org/login', url)
  curlHandle <- getCurlHandle(cookiefile=c_file, cookiejar=c_file)    
  postForm(login_page, 
           .params=list(credential_0=username, 
              credential_1=password, destination='/login'), curl=curlHandle)
  
  data <- getURL(url, curl=curlHandle,ssl.verifypeer = FALSE)
  
  # cleanup
  rm(curlHandle)
  file.remove(c_file)
  gc()
  return(data)
}

R_IADCS_PortalAuth <- function( path= "./" ) {  
  auth_file = paste( path, "auth.txt", sep="")
  if ( file.exists(auth_file) == FALSE ) {
    stop( paste("Missing Authentication configuration file: PATH: ", path))
  }
  c <- paste(read.csv(file=auth_file, as.is = TRUE), collapse = ":")
  cat(c)    
  return (c)
}



