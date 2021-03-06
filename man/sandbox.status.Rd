\name{sandbox.status}
\alias{sandbox.status}
\title{Show sandboxing status}
\usage{
sandbox.status()
}
\value{
The function invisibly returns the sandbox root directory
path (\code{sandbox.dir}).  If sandboxing is inactive, this
is an empty character string (\code{""}).
}
\description{
This function prints the current sandbox settings. It has
no parameters.
}
\examples{
\dontrun{
sandbox.status()
}
}
\seealso{
\code{\link[roxyPackage:sandbox]{sandbox}} to change these
settings.
}

