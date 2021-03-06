\name{roxy.package}
\alias{roxy.package}
\title{Automatic doc creation, package building and repository update}
\usage{
roxy.package(pck.source.dir, pck.version, pck.description, R.libs, repo.root,
  pck.date = Sys.Date(), actions = c("roxy", "package"),
  local.roxy.dir = NULL, cleanup = FALSE, roxy.unlink.target = TRUE,
  rm.vignette = FALSE, R.homes = R.home(),
  html.index = "Available R Packages", html.title = "R package",
  Rcmd.options = c(install = "", build = "--no-manual --no-build-vignettes",
  check = "--as-cran", Rd2pdf = "--pdf --no-preview"), URL = NULL,
  deb.options = NULL, ChangeLog = list(changed = c("initial release"), fixed
  = c("missing ChangeLog")), Rbuildignore = NULL, Rinstignore = NULL, ...)
}
\arguments{
  \item{pck.source.dir}{Character string, path pointing to
  the root directory of your package sources.}

  \item{pck.version}{Character string, defining the
  designated version number. Can be omitted if actions
  don't include \code{"roxy"}, then this information is
  read from the present DESCRIPTION file.}

  \item{pck.description}{Data frame holding the package
  description (see Examples section).}

  \item{R.libs}{Character string, valid path to the R
  library where the package should be installed to.}

  \item{repo.root}{Character string, valid path to a
  directory where to build/update a local package
  repository.}

  \item{pck.date}{Character string of the release date in
  YYYY-MM-DD format. Defaults to \code{Sys.Date()}. If
  actions don't include \code{"roxy"}, then this
  information is read from the present DESCRIPTION file.}

  \item{actions}{Character vector, must contain at least
  one of the following values: \describe{
  \item{"roxy"}{Roxygenize the docs} \item{"cite"}{Update
  CITATION file} \item{"license"}{Update LICENSE.txt file;
  it's not called LICENSE to prevent an automatic
  installation} \item{"check"}{Do a full package check,
  calling \code{R CMD check}} \item{"package"}{Build &
  install the package, update source repository, calling
  \code{R CMD build} and \code{R CMD INSTALL}}
  \item{"cl2news"}{Try to convert a ChangeLog file into an
  NEWS.Rd file} \item{"news2rss"}{Try to convert
  \code{inst/NEWS.Rd} into an RSS feed. You must also set
  \code{URL} accordingly.} \item{"doc"}{Update PDF
  documentation and vignette (if present), \code{R CMD
  Rd2pdf} (or \code{R CMD Rd2dvi} for R < 2.15)}
  \item{"html"}{Update HTML index files}
  \item{"win"}{Update the Windows binary package}
  \item{"macosx"}{Update the Mac OS X binary package}
  \item{"log"}{Generate initial ChangeLog or update a
  present ChangeLog file} \item{"deb"}{Update the Debian
  binary package with
  \code{\link[roxyPackage:debianize]{debianize}} (works
  only on Debian systems; see \code{deb.options}, too)}
  \item{"cleanRd"}{Insert line breaks in Rd files with
  lines longer than 90 chars} } Note that \code{"cl2news"}
  will write the \code{NEWS.Rd} file to the \code{inst}
  directory of your sources, which will overwrite an
  existing file with the same name! Also note that if both
  a \code{NEWS/NEWS.Rd} and \code{ChangeLog} file are
  found, only news files will be linked by the
  \code{"html"} action.}

  \item{local.roxy.dir}{Character string, path to a
  directory to roxygenize docs in, if you don't want to do
  it in place.  If \code{NULL} (the default) or identical
  to \code{pck.source.dir}, docs will be created in place.}

  \item{cleanup}{Logical, if \code{TRUE} will remove backup
  files (matching \code{.*~$} or \code{.*backup$}) from the
  source directory.}

  \item{roxy.unlink.target}{Logical, setting the
  \code{unlink.target} option of
  \code{\link[roxygen2:roxygenize]{roxygenize}}}

  \item{rm.vignette}{Logical, if \code{TRUE} and a vignette
  PDF was build during the \code{"doc"} action, it will not
  be kept in the source package but just be moved to the
  \code{./pckg/$PACKAGENAME} directory of the repository.}

  \item{R.homes}{Path to the R installation to use. Can be
  set manually to build packages for other R versions than
  the default one, if you have installed them in parallel.
  Should probably be used together with \code{R.libs}.}

  \item{html.index}{A character string for the headline of
  the global index HTML file.}

  \item{html.title}{A character string for the title tag
  prefix of the package index HTML file.}

  \item{Rcmd.options}{A named character vector with options
  to be passed on to the internal calls of \code{R CMD
  build}, \code{R CMD INSTALL}, \code{R CMD check} and
  \code{R CMD Rd2pdf} (or \code{R CMD Rd2dvi} for R <
  2.15). Change these only if you know what you're doing!
  Will be passed on as given here. To deactivate, options
  must explicitly be se to \code{""}, missing options will
  be used with the default values.}

  \item{URL}{A character string defining the URL to the
  root of the repository (i.e., which holds the directories
  \code{src} etc.). This is not the path to the local file
  system, but should be the URL to the repository as it is
  available via internet. This option is neccessary for
  (and only interpreted by) the action \code{"news2rss"}.}

  \item{deb.options}{A named list with parameters to pass
  through to
  \code{\link[roxyPackage:debianize]{debianize}}. By
  default, \code{pck.source.dir} and \code{repo.root} are
  set to the values given to the parameters above. As for
  the other options, if not set, the defaults of
  \code{debianize} will be used.}

  \item{ChangeLog}{A named list of character vectors with
  log entry items. The element names will be used as
  section names in the ChangeLog entry, and each character
  string in a vector will be pasted as a log item. The news
  you provide here will be appended to probably present
  news, while trying to prevent duplicate entries to
  appear. If you need more control, don't use the
  \code{"log"} action, but have a look at
  \code{\link[roxyPackage:updateChangeLog]{updateChangeLog}}.
  Also note that the date of altered entries will be
  updated automatically, unless you don't call the
  \code{"roxy"} action, too.}

  \item{Rbuildignore}{A character vector to be used as
  lines of an \code{.Rbuildignore} file. If set, this will
  replace an existing \code{.Rbuildignore} file. Setting it
  to an empty string (\code{""}) will remove the file, the
  default value \code{NULL} will simply keep the file, if
  one is present.}

  \item{Rinstignore}{A character vector to be used as lines
  of an \code{.Rinstignore} file. If set, this will replace
  an existing \code{.Rinstignore} file. Setting it to an
  empty string (\code{""}) will remove the file, the
  default value \code{NULL} will simply keep the file, if
  one is present.}

  \item{...}{Additional options passed through to
  \code{roxygenize}.}
}
\description{
This function should help to create R packages with full
documentation and updates to a local repository. It
supports source and binary packaging (Windows and Mac OS X;
see Note section on the limitations).
}
\details{
For the documentation \code{roxygen2}[1] is used. Next to
the actual in-line documentation of the package's contents,
you only need to prepare a data.frame to be used to write a
package \code{DESCRIPTION} file. See the example section
for details on that. This means that you \emph{neither}
edit the \code{DESCRIPTION} \emph{nor} the
\code{*-package.R} file manually, they will both be created
\emph{automatically} by this function with contents
according to these settings!
}
\note{
The binary packaging is done simply by zipping (Windows) or
targzipping (Mac OS X) the built and installed package.
This should do the trick as long as your package is written
in pure R code. It will most likely not produce usable
packages if it contains code in other languages like C++.
}
\section{Sandboxing}{
  If you want to check out the effects of roxy.package()
  without touching you actual package sources, try
  \code{\link[roxyPackage:sandbox]{sandbox}} to set up a
  safe testing environment.
}

\section{Repository layout}{
  The repository will have this directory structure, that
  is, below the defined \code{repo.root}:

  \describe{ \item{\code{./src/contrib}}{Here go the source
  packages}
  \item{\code{./bin/windows/contrib/$RVERSION}}{Here go the
  Windows binaries}
  \item{\code{./bin/macosx/leopard/contrib/$RVERSION}}{Here
  go the Mac OS X binaries}
  \item{\code{./pckg/index.html}}{A global package index
  with links to packages' index files, if actions included
  \code{"html"}} \item{\code{./pckg/web.css}}{A CRAN-style
  CSS file, if actions included \code{"html"}}
  \item{\code{./pckg/$PACKAGENAME}}{Here go documentation
  PDF and vignette, as well as a \code{ChangeLog} file, if
  found.  and an \code{index.html} with package
  information, if actions included \code{"html"}.  This is
  probably a bit off-standard, but practical if you several
  packages.} }
}

\section{Converting ChangeLogs into NEWS}{
  See \code{\link[roxyPackage:cl2news]{cl2news}} for
  details.
}

\section{Build for several R versions}{
  The options \code{R.libs} and \code{R.homes} can actually
  take more than one string, but a vector of strings. This
  can be used to build packages for different R versions,
  provided you installed them on your system. If you're
  running GNU/Linux, an easy way of doing so is to fetch
  the R sources from CRAN, calling \code{"./configure"}
  with something like \code{"--prefix=$HOME/R/<R
  version>"}, so that \code{"make install"} installs to
  that path. Let's assume you did that with R 2.12.2 and
  2.11.1, you could then call \code{roxy.package} with
  options like \code{R.homes=c("home/user/R/R-2.11.1",
  "home/user/R/R-2.12.2")} and
  \code{R.libs=c("home/user/R/R-2.11.1/lib64/R/library",}
  \code{"home/user/R/R-2.12.2/lib64/R/library")}.
  \code{roxy.package} will then call itself recursively for
  each given R installation.

  One thing you should be aware of is that
  \code{roxy.package} will not perform all actions each
  time. That is because some of them, namely \code{"roxy"},
  \code{"cite"}, \code{"license"}, \code{"doc"},
  \code{"cl2news"} and \code{"news2rss"}, should produce
  identical results anyway, so they are only considered
  during the first run. You should always place the R
  version which should be linked to from the HTML index
  last in line, because \code{"html"} will overwrite
  previous results. For a similar reason, the \code{"deb"}
  action will only actually build a binary package during
  the last run, but debianizing it will be done during the
  first.
}

\section{Windows}{
  On Windows, the actions \code{"doc"} and \code{"check"}
  will only work correctly if you have installed and
  configured LaTeX accordingly, and you will also need
  Rtools set up for packaging.
}

\section{CRAN compliance}{
  The CRAN policies can sometimes be very strict. This
  package should allow you to produce packages which are
  suitable for release on CRAN. But some steps have to be
  taken care of by yourself. For instance, CRAN does
  currently not allow copies of common licenses in a source
  package, nor a \code{debian} folder. Therefore, if your
  package is supposed to be released on CRAN, you should
  include \code{Rbuildignore=c("debian", "LICENSE.txt")} to
  the function call.
}
\examples{
\dontrun{
## package description as data.frame:
pckg.dscrptn <- data.frame(
  Package="SquareTheCircle",
  Type="Package",
  Title="Squaring the circle using Heisenberg compensation",
  Author="E.A. Dölle <doelle@eternalwondermaths.example.org>",
  AuthorR="c(person(given=\\"Ernst\\", family=\\"Dölle\\",
  email=\\"doelle@eternalwondermaths.example.org\\", role=c(\\"aut\\", \\"cre\\")))",
  Maintainer="E.A. Dölle <doelle@eternalwondermaths.example.org>",
  Depends="R (>= 2.10.0),heisenberg (>= 0.23),tools",
  Enhances="rkward",
  Description="This package squares the circle using Heisenberg compensation.
      The code came from a meeting with Yrla Nor that i had in a dream. Please
      don't forget to chain your computer to the ground, because these
      algorithms might make it fly.",
  License="GPL (>= 3)",
  Encoding="UTF-8",
  LazyLoad="yes",
  URL="http://eternalwondermaths.example.org",
  stringsAsFactors=FALSE)
# hint no. 1: you *don't* specify version number and release date here,
#   but all other valid fields for DESCRIPTION files must/can be defined
# hint no. 2: most of this rarely changes, so you can add this to the
#   internals of your package and refer to it as
#   roxy.package(pck.description=SquareTheCircle:::pckg.dscrptn, ...)
# hint no. 3: use "AuthorR" for the "Author@R" field, or "AuthorsR" for
# R >= 2.14, to work around naming problems

roxy.package(pck.source.dir="~/my_R_stuff/SquareTheCircle",
  pck.version="0.01-2",
  pck.description=pckg.dscrptn,
  R.libs="~/R",
  repo.root="/var/www/repo",
  actions=c("roxy", "package", "doc"))
}
}
\references{
[1] \url{http://cran.r-project.org/web/packages/roxygen2/}
}
\seealso{
\code{\link[roxyPackage:sandbox]{sandbox}} to run
roxy.package() in a sandbox.
}

