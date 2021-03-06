\name{entities}
\alias{entities}
\title{Translate character string into HTML entities}
\usage{
entities(string, collapse = TRUE)
}
\arguments{
  \item{string}{Character string to be translated.}

  \item{collapse}{Logical, if \code{TRUE} one single string
  is returned}
}
\value{
Either a named character vector, one element for each
character, or a single string.
}
\description{
Translate character string into HTML entities
}
\examples{
entities("foo_bar")
}

