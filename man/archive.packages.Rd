\name{archive.packages}
\alias{archive.packages}
\title{Deal with old packages in your local repository}
\usage{
archive.packages(repo.root, to.dir = "Archive", keep = 1, package = NULL,
  type = "source", archive.root = repo.root, overwrite = FALSE,
  reallyDoIt = FALSE)
}
\arguments{
  \item{repo.root}{Path to the repository root, i.e., the
  directory which contains the \code{src} and \code{bin}
  directories. Usually this path should start with
  "\code{file:///}" except for Windows.}

  \item{to.dir}{Character string, name of the folder to
  move the old packages to.}

  \item{keep}{An integer value defining the maximum nuber
  of versions to keep. Setting this to 0 will completely
  remove all packages from the repository, which is
  probably only useful in combination with the option
  \code{package}.}

  \item{package}{A character vector with package names to
  check. If set, \code{archive.packages} will only take
  actions on these packages. If \code{NULL}, all packages
  are affected.}

  \item{type}{A character vector defining the package
  formats to keep. Valid entries are \code{"source"},
  \code{"win.binary"} and \code{"mac.binary.leopard"}. By
  default, only the source packages are archived, all other
  packages are deleted.}

  \item{archive.root}{Path to the archive root, i.e., the
  directory to which files should be moved. Usually the
  Archive is kept i \code{repo.root}}

  \item{overwrite}{Logical, indicated whether existing
  files in the archive can be overwritten.}

  \item{reallyDoIt}{Logical, real actions are only taken if
  set to \code{TRUE}, otherwise the actions are only
  printed.}
}
\description{
Use this function to move older versions of a package to a
specified archive directory, or remove them completely.
}
\note{
This function responds to
\code{\link[roxyPackage:sandbox]{sandbox}}.
}
\examples{
\dontrun{
# dry run, only prints what would happen, so you can check
# if that's really what you want
archive.packages("file:///var/www/repo")

# after we've confirmed that the right packages will be moved
# and deleted, let's actually commit the changes
archive.packages("file:///var/www/repo", reallyDoIt=TRUE)

# if we don't want a standard archive, but for instance a parallel
# archive repository, we can have it. let's move all but the latest two
# versions from /var/www/repo to /var/www/archive. to suppress the
# creation of a special archive directory, we set to.dir=""
archive.packages("file:///var/www/repo", to.dir="", keep=2,
  type=c("source", "win.binary", "mac.binary.leopard"),
  archive.root="/var/www/archive", reallyDoIt=TRUE)
}
}
\seealso{
\code{\link[roxyPackage:sandbox]{sandbox}} to run
archive.packages() in a sandbox.
}

