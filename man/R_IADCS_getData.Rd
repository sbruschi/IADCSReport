\name{R_IADCS_getData}
\alias{R_IADCS_getData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
What the package does (short line)
~~ package title ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
R_IADCS_getData(url, userpwd = NA, path = "./")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{
%%     ~~Describe \code{url} here~~
}
  \item{userpwd}{
%%     ~~Describe \code{userpwd} here~~
}
  \item{path}{
%%     ~~Describe \code{path} here~~
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
function (url, userpwd = NA, path = "./") 
{
    R_IADCS_autorize_autorization = get("R_IADCS_autorize_autorization", 
        pkg_globals)
    print("R_IADCS_autorize_autorization")
    if (R_IADCS_autorize_autorization == FALSE) {
    }
    print(url)
    userpass <- R_IADCS_PortalAuth(path)
    if (is.na(userpwd) == FALSE) {
        userpass <- userpwd
    }
    account <- unlist(strsplit(userpass, ":"))
    username <- account[1]
    password <- account[2]
    c_file <- tempfile()
    if (file.exists(c_file) == FALSE) {
        file.create(c_file)
    }
    login_page <- sub("(iadcs.org\\b)(.)*", "iadcs.org/login", 
        url)
    curlHandle <- getCurlHandle(cookiefile = c_file, cookiejar = c_file)
    postForm(login_page, .params = list(credential_0 = username, 
        credential_1 = password, destination = "/login"), curl = curlHandle)
    data <- getURL(url, curl = curlHandle, ssl.verifypeer = FALSE)
    rm(curlHandle)
    file.remove(c_file)
    gc()
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
