\name{write_PACKAGES}
\alias{write_PACKAGES}
\title{Generate PACKAGES files}
\usage{
write_PACKAGES(dir = ".", type = c("source", "mac.binary", "win.binary"),
  verbose = FALSE, subdirs = FALSE, latestOnly = TRUE, ...)
}
\arguments{
  \item{dir}{Character vector describing the location of
  the repository (directory including source or binary
  packages) to generate the 'PACKAGES' and 'PACKAGES.gz'
  files from and write them to.}

  \item{type}{Type of packages: currently source
  ‘.tar.gz’ archives, and Mac or Windows binary
  (‘.tgz’ or ‘.zip’, respectively) packages are
  supported. Defaults to "win.binary" on Windows and to
  "source" otherwise.}

  \item{verbose}{logical. Should packages be listed as they
  are processed?}

  \item{subdirs}{either logical (to indicate if
  subdirectories should be included, recursively) or a
  character vector of name of subdirectories to include.}

  \item{latestOnly}{logical: if multiple versions of a
  package are available should only the latest version be
  included? (\code{\link{archive.packages}} only works for
  source packages when \code{latestOnly} is \code{FALSE})}

  \item{...}{additional arguments passed to
  \code{\link[tools]{write_PACKAGES}}}
}
\description{
Generate 'PACKAGES' and 'PACKAGES.gz' files for a
repository of source or Mac/Windows binary packages. This
directly calls \code{\link[tools]{write_PACKAGES}}.
}
\details{
In addition, it has the following enhancements: \enumerate{
\item Fetch files on Network Drive to tmp folder before
producing INDEX }
}
\seealso{
\code{\link[tools]{write_PACKAGES}}
}

