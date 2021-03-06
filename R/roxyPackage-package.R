#' Utilities to automate package builds.
#'
#' \tabular{ll}{
#' Package: \tab roxyPackage\cr
#' Type: \tab Package\cr
#' Version: \tab 0.03-12\cr
#' Date: \tab 2014-03-15\cr
#' Depends: \tab R (>= 2.9.0),methods,roxygen2,XiMpLe (>= 0.03-20)\cr
#' Encoding: \tab UTF-8\cr
#' License: \tab GPL (>= 3)\cr
#' LazyLoad: \tab yes\cr
#' URL: \tab http://reaktanz.de/?c=hacking&s=roxyPackage\cr
#' }
#'
#' The intention of this package is to make packaging R code as
#' easy as possible. roxyPackage uses tools from the roxygen2 package to generate documentation. It also automatically
#' generates and updates files like *-package.R, DESCRIPTION, CITATION, ChangeLog and NEWS.Rd. Building packages
#' supports source format, as well as several binary formats (MS Windows, Mac OS X, Debian GNU/Linux) if the
#' package contains pure R code only. The packages built are stored in a fully functional local R package repository
#' which can be synced to a web server to share them with others. This includes the generation of browsable HTML
#' pages similar to CRAN, with support for RSS feeds from the ChangeLog. Please read the vignette for a more detailed
#' explanation by example.
#'
#' @aliases roxyPackage-package
#' @name roxyPackage-package
#' @docType package
#' @title The roxyPackage Package
#' @author m.eik michalke
#' @keywords package
NULL
