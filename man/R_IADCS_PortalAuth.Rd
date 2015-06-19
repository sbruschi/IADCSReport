\name{R_IADCS_PortalAuth}
\alias{R_IADCS_PortalAuth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
What the package does (short line)
~~ package title ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
R_IADCS_PortalAuth(path = "./")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function (path = "./") 
{
    auth_file = paste(path, "auth.txt")
    if (file.exists(auth_file) == FALSE) {
        stop(paste("Missing Authentication configuration file: PATH: ", 
            path))
    }
    c <- paste(read.csv(file = auth_file, as.is = TRUE), collapse = ":")
    cat(c)
    return(c)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
