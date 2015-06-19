\name{R_IADCS_autorize_server}
\alias{R_IADCS_autorize_server}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
What the package does (short line)
~~ package title ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
R_IADCS_autorize_server(session = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{session}{
%%     ~~Describe \code{session} here~~
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
function (session = NULL) 
{
    query <- parseQueryString(session$clientData$url_search)
    token <- query[["token"]]
    token_api_url <- paste("https://jia.iadcs.org/cgi-bin/shiny_service.pl?token=", 
        token, sep = "")
    referer <- paste("https://", session$clientData$hostname, 
        sep = "")
    data <- getURL(token_api_url, ssl.verifypeer = FALSE, REFERER = session$clientData$url_hostname, 
        httpheader = c(Shiny_report = session$clientData$url_pathname))
    data_json <- fromJSON(data)
    print(data_json$valid_user)
    if (data_json$valid_user == TRUE) {
        assign("R_IADCS_autorize_autorization", TRUE, pkg_globals)
    }
    else {
        assign("R_IADCS_autorize_autorization", FALSE, pkg_globals)
    }
    return(data_json)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
