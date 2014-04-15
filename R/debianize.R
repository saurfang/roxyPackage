# Copyright 2011-2014 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package roxyPackage.
#
# roxyPackage is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# roxyPackage is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with roxyPackage.  If not, see <http://www.gnu.org/licenses/>.


#' Basic Debian package creation from R source packages
#' 
#' This function attempts to 'debianize' your R source package. This means, it will add a \code{debian} directory
#' to sources' root directory, and populate it with needed files for Debian package building, as outlined in the Debian R Policy
#' by Eddelbuettel & Bates (2003) and the Debian Policy Manual[1], version 3.9.3.1.
#' 
#' The file \code{./debian/source/format} will also be created only once. The files \code{./debian/control}, \code{./debian/copyright} and
#' \code{./debian/rules} will be generated from the information found in the \code{DESCRIPTION} file of the R package.
#' Once created, these files won't be touched again if they are not defined in the \code{overwrite} parameter. This enables you to save
#' files from being re-written, e.g. if you altered them manually.
#' 
#' The \code{./debian/changelog} is special here, as \code{overwrite} doesn't mean the whole file will be overwritten, but rather that the
#' function checks if the changelog already contains an entry for this particular package version and revision, and only if this is not the
#' case will add one at the beginning of the file, including the log entries defined by the \code{changelog} parameter (each string will
#' become one log entry).
#' 
#' The function will try to detect the license you specified in the \code{DESCRIPTION} file, and if it is one of the following licenses,
#' generate some useful info on how to get the full license on a Debian system:
#' \itemize{
#'    \item{Apache License}
#'    \item{Artisitic License}
#'    \item{BSD License}
#'    \item{GNU General Public License (GPL)}
#'    \item{GNU Lesser General Public License (LGPL)}
#' }
#' 
#' @section Building the actual package: If you're running the R session on a Debian based system, the function can build the debian package,
#' but it would likely fail when it comes to signing the .changes/.dsc files, because \code{gpg} gets invoked without \code{"--no-tty"}.
#' You'd have to sign those files later, e.g. with \code{debsign}, if you really need this. However, secure-apt can still be ensured, if you provide
#' a valid GnuPG key ID from your keyring, which will then be used to sign the generated \code{Release} file. If not present yet, a copy of
#' the public key will automatically be saved to the repository, in a file named \code{<key ID>.asc}.
#' 
#' Package building is done in a temporal directory, and the source files a copied there first. Set \code{build.dir=pck.source.dir} if
#' you want to build in-place instead.
#' 
#' @section Package dependencies: This function will make no attempts to guess what package dependencies must be fulfilled.
#' That is, if the defaults don't fit (see below), then you must define these dependencies yourself via the \code{deb.description}
#' parameter (setting appropriate values for fields like \code{Build.Depends}, \code{Build.Depends.Indep} and \code{Depends}). In case your R package
#' depends on other R packages, you will have to ensure that these are also available as Debian packages (and define them
#' as dependencies), so the package management can take care of resolving these dependencies transparently. Otherwise users might
#' have a hard time figuring out how to get your package to work, if the building process doesn't fail in the first place.
#' 
#' That said, you should always try to debianize the package without manual dependencies set first. After that, look at the generated \code{control}
#' file and see if there are problems at all. Usually the default method is supposed to be quite clever when it comes to detect dependencies from the
#' actual package \code{DESCRIPTION} file (it will automatically translate those into proper Debain package names, where tuning is possible via the
#' \code{depends.origin} and \code{depends.origin.alt} parameters).
#' 
#' @section Repository access: After you debianized your package and built some Debian packages, \code{debianize} will prepare a Debain package repository
#' in the specified directory (can be the same as used with \code{roxy.package}). You can now access it locally on your machine, or upload the whole thing
#' to a web server etc. Basically, it should work if you add these lines to your repository configuration:
#' 
#' \code{deb http://<URL you uploaded to>/deb <distribution> <component>}
#'
#' \code{deb-src http://<URL you uploaded to>/deb <distribution> <component>}
#' 
#' @section Debianizing arbitrary packages: With a little luck, this function can almot automatically debianize any R package sources. You can even provide
#' the \code{pck.source.dir} parameter with a URL to package sources (e.g., a source package from CRAN), and \code{debianize} will do its best to end up
#' with an installable debian package in the specified repository root.
#'
#' @note Please note that the package will always be built against the R version installed by your package management!
#' Also, this function responds to \code{\link[roxyPackage:sandbox]{sandbox}}.
#'
#' @param pck.source.dir Character string, path pointing to the root directory of your package sources, to a local R package source tarball, or
#'    a full URL to such a package tarball. Tarballs will be downloaded to a temporary directory, if needed, extracted, and then debianized.
#' @param repo.root Character string, valid path to a directory where to build/update a local package repository.
#' @param build.dir Character string, valid path to a directory where to build the package.
#' @param revision Numeric or a character string, the Debian package revision information.
#' @param origin Character string, should be either "noncran" or "other-<yourname>", used for the package name. This indicates that your package is
#'    not an official CRAN or BioC package.
#' @param distribution Character string, the Debain (based) distribution your package is intended for.
#' @param component Character string, the Debain component of the distribution.
#' @param urgency Character string, urgency information for this release (refer to [1] if you want to change this).
#' @param changelog Character vector, log entries for the \code{./debian/changelog} file if it is going to be changed.
#' @param deb.description A named list or data.frame with further information, especially for the \code{./debian/control} file. This is similar to
#'    the \code{pck.description} parameter of \code{\link[roxyPackage:roxy.package]{roxy.package}}, only with different variables.
#'    Note that if certain key values are missing, \code{debianize} will automatically use some defaults:
#'    \describe{
#'      \item{Build.Depends.Indep}{\code{"debhelper (>> 7.0.0), r-base-dev (>= <R.vers>), cdbs"}, plus Depends/Imports in \code{DESCRIPTION} in debianized format;
#'        if \code{arch} is not set to \code{"all"}, the field \code{Build.Depends} is used instead}
#'      \item{Depends}{\code{"r-base-core (>= <R vers>)"}, plus Depends/Imports in \code{DESCRIPTION} in debianized format}
#'      \item{Suggests}{Suggests in \code{DESCRIPTION} in debianized format}
#'      \item{Maintainer}{generated from \code{\link[base:Sys.info]{Sys.info}} (\code{user <login@@nodename>}), with a warning.}
#'      \item{Section}{\code{"math"}}
#'      \item{Priority}{\code{"optional"}}
#'      \item{Homepage}{URL in \code{DESCRIPTION}}
#'    }
#'    Refer to [1] for further available fields in the \code{./debian/control} file. In case you would like to add to the fields definig relations to other packages
#'    like \code{Build.Depends} or \code{Depends} rather than replacing them, provide a named character vector with a value called "append". For example:
#'    \code{Depends=c(append="libmysql++3")}.
#' @param actions Character vector, naming the actions to perform:
#'    \describe{
#'      \item{"deb"}{Debianize the package sources.}
#'      \item{"bin"}{Build the Debian package.}
#'      \item{"src"}{Build a Debian source package (currently not implemented).}
#'    }
#' @param overwrite Character vector, naming the files which should be updated:
#'    \describe{
#'      \item{"changelog"}{Update \code{./debian/changelog}, but only if no entry for this package version and revision is there yet}
#'      \item{"control"}{Re-write \code{./debian/control}}
#'      \item{"copyright"}{Re-write \code{./debian/copyright}}
#'      \item{"rules"}{Re-write \code{./debian/rules}}
#'      \item{"gpg.key"}{Re-write \code{<key ID.asc>} in the repository (by default present keys are left unchanged)}
#'    }
#' @param depends.origin A character string to set the default origin for packages which are a dependency of this one. In case all dependencies can be
#'    met by Debian packages from CRAN releases, you can leave this to the default setting. If you need more control, see \code{depends.origin.alt}.
#' @param depends.origin.alt A named list of alternative origins for packages which are a dependency of this one. By default, \code{depends.origin} is used,
#'    but if you know that certain dependencies are of different origin (e.g., your own repository), you can set this here. Each list element must be named after
#'    the R package you want to set an origin for, and must be a character vector or single string, like \code{list(foo="other-janedoe")}. If more than one origin
#'    is given, they will be set as alternatives (using the pipe \code{"|"} as "or").
#' @param bin.opts Character string, options to pass through to \code{dpkg-buildpackage} for the \code{"bin"} action.
#' @param arch Character string, architecture the package is build for.
#' @param compat Integer value, specifying the \code{debhelper} compatibility level.
#' @param epoch Integer value, the Debian package epoch information.
#' @param gpg.key Character string, the GnuPG key ID for the key that should be used for signing the Release file (secure apt).
#'    This key must be available in your keyring. Skipped if \code{NULL}.
#' @param keep.build Logical. If \code{build.dir} is not \code{pck.source.dir}, work is done in generated folder with a random name. Usually it
#'    is removed afterwards, unless you set this option to \code{TRUE}.
#' @param replace.dots Logical. The proposed Debian R Policy actually asks to replace all dots in package names by hyphens. However,
#'    this is implemented differently in \code{r-cran.mk} and will lead to unbuildable packages. So the default here is to ignore the policy draft and keep dots
#'    in package names, as is true for a lot of CRAN packages as well (code is law). In case you run into problems here
#'    (symptoms include a failing .deb build because the directory \code{build/<package name>} doesn't exist), try turning this switch. If \code{TRUE}
#'    dots will be replaced by hyphens in both source and binary package names. Note that building a package by calling this function should always
#'    work, because it will automatically create a symlink in the build directory if needed.
#' @seealso \code{\link[roxyPackage:sandbox]{sandbox}} to run debianize() in a sandbox.
#' @references
#' Eddelbuettel, D. & Bates, D. (2003). \emph{Debian R Policy -- Draft Proposal v 0.1.3}.
#'   Available from \url{http://lists.debian.org/debian-devel/2003/12/msg02332.html}
#'
#' [1] Debian Policy Manual: \url{http://www.debian.org/doc/debian-policy}
#' @export
#' @examples
#' \dontrun{
#' debianize(
#'   pck.source.dir="~/my_R_stuff/SquareTheCircle",
#'   repo.root="/var/www/repo",
#'   origin="other-doelle",
#'   revision=4,
#'   changelog=c("re-compiled docs"),
#'   deb.description=list(
#'     depends=c("r-base-dev (>> 2.12.0), r-cran-foreign"),
#'     maintainer="A. Sistent <sistent@@eternalwondermaths.example.org>"),
#'   actions=c("deb"))
#' }

debianize <- function(
  pck.source.dir,
  repo.root,
  build.dir=tempdir(),
  revision=1,
  origin="other-roxypackage",
  distribution="unstable",
  component="main",
  urgency="low",
  changelog=c("new upstream release"),
  deb.description=NULL,
  depends.origin="cran",
  depends.origin.alt=list(),
  actions=c("deb", "bin", "src"),
  overwrite=c("changelog", "control", "copyright", "rules"),
  bin.opts="-rfakeroot -b -uc",
  arch="all",
  compat=7,
  epoch=NULL,
  gpg.key=NULL,
  keep.build=FALSE,
  replace.dots=FALSE){

  # check if we're about to debianize local sources, a tarbal or
  # something we need to download first
  pck.source.dir <- deb.check.sources(src=pck.source.dir)

  ## check for sandboxing
  if(isTRUE(check.sandbox())){
    message("preparing sandbox...")
    # prepare folder structure; this will also insure sane values and abort
    # if locations are invalid. the function returns a list of paths to use
    adjust.paths <- prepare.sandbox.deb(
      pck.source.dir=pck.source.dir,
      repo.root=repo.root)
    # replace paths with sandbox
    pck.source.dir <- adjust.paths[["pck.source.dir"]]
    repo.root <- adjust.paths[["repo.root"]]
    sandbox.status()
  } else {}

  if(any(c("bin","src") %in% actions)){
    # basic checks
    # can this be a debian system at all?
    if(!isUNIX()){
      stop(simpleError("this doesn't seem to be a UNIX system, so i assume it's not Debain as well!"))
    } else {}
    # are dpkg-buildpackage, fakeroot etc. available?
    # we also need a native tar for the extra arguments to work
    neededTools <- c("dpkg-buildpackage", "fakeroot", "dpkg-source", "dpkg-genchanges", "apt-ftparchive", "tar")
    if(!is.null(gpg.key)){
      neededTools <- c(neededTools, "gpg")
    } else {}
    buildTools <- Sys.which(neededTools)
    missingTools <- buildTools %in% ""
    if(any(missingTools)){
      stop(simpleError(paste0("can't find these tools:\n    '", paste(neededTools[missingTools], collapse="'\n    '"),"'\n  please check your build environment!")))
    }
    if(!identical(build.dir, pck.source.dir)){
      build.dir <- tempfile(tmpdir=build.dir)
      if(!file_test("-d", build.dir)){
        stopifnot(dir.create(build.dir, recursive=TRUE))
        if(!isTRUE(keep.build)){
          on.exit(unlink(build.dir, recursive=TRUE))
        } else {}
        message(paste0("deb: created ", build.dir, "."))
      } else {}
    } else {}
    # create repo structure
    repo.all.arch <- c("binary-i386","binary-amd64")
    repo.deb.path <- file.path(repo.root, "deb")
    repo.gpg.key.file <- file.path(repo.root, paste0(gpg.key, ".asc"))
    repo.arch.rel.paths <- file.path("dists", distribution, component, repo.all.arch)
    repo.arch.paths <- file.path(repo.deb.path, repo.arch.rel.paths)
    repo.bin.rel.path <- file.path("dists", distribution, component, "all")
    repo.bin.path <- file.path(repo.deb.path, repo.bin.rel.path)
    repo.release.path <- file.path(repo.deb.path, "dists", distribution)
    repo.src.pseudo.rel.path <- file.path("dists", distribution, component, "source")
    repo.src.pseudo.path <- file.path(repo.deb.path, repo.src.pseudo.rel.path)
    repo.src.real.rel.path <- file.path("source", distribution)
    repo.src.real.path <- file.path(repo.deb.path, repo.src.real.rel.path)
    for (this.path in c(repo.bin.path, repo.src.pseudo.path, repo.src.real.path, repo.arch.paths)){
      if(!file_test("-d", this.path)){
        stopifnot(dir.create(this.path, recursive=TRUE))
        message(paste0("deb: created ", this.path, " (repository)."))
      } else {}
    }
  } else {}

  # read the description file
  pck.dscrptn <- as.data.frame(read.dcf(file=file.path(pck.source.dir, "DESCRIPTION")), stringsAsFactors=FALSE)
  # clean from newlines
  pck.dscrptn <- as.data.frame(t(sapply(pck.dscrptn, function(x) gsub("\n", " ", x))), stringsAsFactors=FALSE)
  pck.version <- as.character(pck.dscrptn[["Version"]])
  pck.date <- as.character(pck.dscrptn[["Date"]])
  pck.package <- as.character(pck.dscrptn[["Package"]])
  pck.title <- as.character(pck.dscrptn[["Title"]])
  pck.license <- as.character(pck.dscrptn[["License"]])
  if(is.null(pck.dscrptn[["Authors@R"]])){
    pck.maintainer <- as.character(pck.dscrptn[["Maintainer"]])
    pck.author <- as.character(pck.dscrptn[["Author"]])
    pck.author.nomail <- gsub("[[:space:]]*<[^>]*>", "", pck.author)
  } else {
    pck.persons <- as.character(pck.dscrptn[["Authors@R"]])
    pck.maintainer <- format(get.by.role(eval(parse(text=pck.persons)), "cre"), include=c("given", "family", "email"), braces=list(email=c("<", ">")))
    pck.author <- format(get.by.role(eval(parse(text=pck.persons)), "aut"), include=c("given", "family", "email"), braces=list(email=c("<", ">")))
    pck.author.nomail <- format(get.by.role(eval(parse(text=pck.persons)), "aut"), include=c("given", "family"))
  }
  pck.description <- as.character(pck.dscrptn[["Description"]])

  # define some paths
  pck.src.path.parts <- unlist(strsplit(pck.source.dir, .Platform$file.sep))
  pck.src.folder.name <- pck.src.path.parts[length(pck.src.path.parts)]
  deb.dir.debian <- file.path(pck.source.dir, "debian")
  deb.dir.source <- file.path(deb.dir.debian, "source")
  deb.file.format <- file.path(deb.dir.source, "format")
  deb.file.changelog <- file.path(deb.dir.debian, "changelog")
  deb.file.compat <- file.path(deb.dir.debian, "compat")
  deb.file.control <- file.path(deb.dir.debian, "control")
  deb.file.copyright <- file.path(deb.dir.debian, "copyright")
  deb.file.rules <- file.path(deb.dir.debian, "rules")

  # deb.pckg.name.lower is needed to work around possible implementation bugs, see below!
  deb.pckg.name.lower <- debianPkgName(package=pck.package, origin=origin, version=NULL, replace.dots=FALSE)
  deb.pckg.name <- deb.srcs.name <- debianPkgName(package=pck.package, origin=origin, version=NULL, replace.dots=replace.dots)
  deb.pckg.vers <- paste(pck.version, revision, sep="-")
  if(!is.null(epoch)){
    deb.pckg.vers <- paste(epoch, deb.pckg.vers, sep=":")
  } else {}

  thisYear <- format(Sys.Date(), "%Y")

  ## debianize: create & populate debian directory
  if("deb" %in% actions){
    # we'll try the quilt way:
    # debian/
    #   source/
    #     format       (includes only the line "3.0 (quilt)")
    #   changelog
    #   control
    #   compat
    #   copyright
    #   rules

    # check for/create directories
    if(!file_test("-d", deb.dir.source)){
      stopifnot(dir.create(deb.dir.source, recursive=TRUE))
      message(paste0("deb: created ", deb.dir.source, "."))
    } else {}
    if(!file_test("-f", deb.file.format)){
      cat("3.0 (quilt)\n", file=deb.file.format)
      message(paste0("deb: created ", deb.file.format, " (set to quilt format)."))
    } else {}

    # check missing contents of deb.description and set defaults
    thisRVers <- paste(getRvers(R.homes=R.home(), win=TRUE), "0", sep=".")

    if(is.null(deb.description)){
      deb.description <- list()
    }
    if(is.null(deb.description[["Depends"]]) || check.append(deb.description[["Depends"]])){
      # generate alternative debian package dependencies
      deb.description[["Depends"]] <- debianizeDepends(dep=c(as.character(pck.dscrptn[["Depends"]]),as.character(pck.dscrptn[["Imports"]])), origin=depends.origin,
          origin.alt=depends.origin.alt, forceRVersion=thisRVers, append=check.append(deb.description[["Depends"]], value=TRUE), replace.dots=replace.dots)
    } else {
      deb.description[["Depends"]] <- paste(deb.description[["Depends"]], collapse=", ")
    }
    # if arch is "all", use Build.Depends.Indep, else Build.Depends
    build.dep.field <- ifelse(identical(arch, "all"), "Build.Depends.Indep", "Build.Depends")
    if(is.null(deb.description[[build.dep.field]]) || check.append(deb.description[[build.dep.field]])){
      deb.description[[build.dep.field]] <- paste0(paste0("debhelper (>> 7.0.0), r-base-dev (>= ", thisRVers, "), cdbs"),
        gsub("r-base-core[^,]*", "", deb.description[["Depends"]]),
        paste(check.append(deb.description[[build.dep.field]], value=TRUE), collapse=", "))
    } else {
      deb.description[[build.dep.field]] <- paste(deb.description[[build.dep.field]], collapse=", ")
    }
    if(is.null(deb.description[["Suggests"]]) || check.append(deb.description[["Suggests"]])){
      if(!is.null(pck.dscrptn[["Suggests"]])){
        deb.description[["Suggests"]] <- debianizeDepends(dep=as.character(pck.dscrptn[["Suggests"]]), origin=depends.origin,
            origin.alt=depends.origin.alt, forceRVersion=NULL, append=check.append(deb.description[["Suggests"]], value=TRUE), replace.dots=replace.dots)
      } else {
        deb.description[["Suggests"]] <- paste(check.append(deb.description[["Suggests"]], value=TRUE), collapse=", ")
      }
    } else {
      deb.description[["Suggests"]] <- paste(deb.description[["Suggests"]], collapse=", ")
    }
    if(is.null(deb.description[["Maintainer"]])){
      # try to generate a half-way reasonable maintainer ID
      system.info <- Sys.info()
      system.login <- Sys.getenv("LOGNAME")
      deb.description[["Maintainer"]] <- paste0(system.info[["user"]], " <", system.login, "@", system.info[["nodename"]], ">")
      warning(paste0("deb: you didn't specify a Debian package maintainer, it was set to \"", deb.description[["Maintainer"]], "\"!"), call.=FALSE)
    } else {}
    if(is.null(deb.description[["Section"]])){
      deb.description[["Section"]] <- "math"
    } else {}
    if(is.null(deb.description[["Priority"]])){
      deb.description[["Priority"]] <- "optional"
    } else {}

    ## debian/compat
    if(!file_test("-f", deb.file.compat) | "compat" %in% overwrite){
      stopifnot(is.numeric(compat))
      cat(compat, "\n", file=deb.file.compat)
      message(paste0("deb: created ", deb.file.compat, " (set to level ", compat, ")."))
    } else {}

    ## debian/control
    if(!file_test("-f", deb.file.control) | "control" %in% overwrite){
      deb.txt.control.src <- data.frame(
        Source=deb.srcs.name,
        Section=deb.description[["Section"]],
        Priority=deb.description[["Priority"]],
        Maintainer=deb.description[["Maintainer"]],
        stringsAsFactors=FALSE)
      # workaround, didn't manage to escape the special chars otherwise
      if("Build.Depends.Indep" %in% names(deb.description)){
        deb.txt.control.src$`Build-Depends-Indep` <- deb.description[["Build.Depends.Indep"]]
      } else {}
      if("Build.Depends" %in% names(deb.description)){
        deb.txt.control.src$`Build-Depends` <- deb.description[["Build.Depends"]]
      } else {}
      deb.txt.control.src$`Standards-Version` <- "3.9.3.1"

      deb.txt.control.pck <- data.frame(
        Package=deb.pckg.name,
        Architecture=arch,
        Section=deb.description[["Section"]],
        Depends=deb.description[["Depends"]],
        Description=paste0("GNU R package: ", pck.description),
        stringsAsFactors=FALSE)

      # additional valid fields
      if("Homepage" %in% names(deb.description)){
        deb.txt.control.src[["Homepage"]] <- deb.txt.control.pck[["Homepage"]] <- deb.description[["Homepage"]]
      } else if("URL" %in% names(pck.dscrptn)){
        deb.txt.control.src[["Homepage"]] <- deb.txt.control.pck[["Homepage"]] <- as.character(pck.dscrptn[["URL"]])
      } else {}
      if("Essential" %in% names(deb.description)){
        deb.txt.control.pck[["Essential"]] <- deb.description[["Essential"]]
      } else {}
      if("Suggests" %in% names(deb.description)){
        deb.txt.control.pck[["Suggests"]] <- deb.description[["Suggests"]]
      } else {}
      for (theRest in names(deb.description)[!names(deb.description) %in% c(names(deb.txt.control.pck), names(deb.txt.control.src), "Build.Depends.Indep", "Build.Depends")]){
        deb.txt.control.pck[[theRest]] <- deb.description[[theRest]]
      }

      # write the control file
      write.dcf(deb.txt.control.src, file=deb.file.control, append=FALSE, indent=1)
      cat("\n", file=deb.file.control, append=TRUE)
      write.dcf(deb.txt.control.pck, file=deb.file.control, append=TRUE, indent=1)
      message("deb: debian/control updated.")
    } else {}
    
    ## debian/copyright
    if(!file_test("-f", deb.file.copyright) | "copyright" %in% overwrite){
      if(checkLicence(pck.license)){
        licenseInfo <- checkLicence(pck.license, deb=TRUE, logical=FALSE)
        includeLicense <- paste0(pck.package, " Copyright (C) ", thisYear, " ", pck.author.nomail, ", released under the\n",
        licenseInfo[["name"]],
        if(!is.na(licenseInfo[["version"]])){
          paste0(" version ", licenseInfo[["version"]])
        } else {},
        if(grepl(">", pck.license)){
          paste0(" or (at your option) any later version")
        } else {},
        ".\n\n",
        "This software is distributed in the hope that it will be useful, but\n",
        "WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY\n",
        "or FITNESS FOR A PARTICULAR PURPOSE.\n\n",
        "You should have received a copy of the license with your Debian system,\n",
        "in the file /usr/share/common-licenses/", licenseInfo[["file"]], ", or with the\n",
        "source package as the file COPYING or LICENSE.\n")
      } else {
        includeLicense <- paste0(pck.package, " Copyright (C) ", thisYear, " ", pck.author.nomail, ", released under the\n",
        "terms of the ", pck.license, " license.\n\n",
        "This software is distributed in the hope that it will be useful, but\n",
        "WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY\n",
        "or FITNESS FOR A PARTICULAR PURPOSE.\n\n",
        "You should have received a copy of the license with the\n",
        "source package as the file COPYING or LICENSE.\n")
      }
      deb.txt.copyright <- paste0(
        if(identical(pck.author, pck.maintainer)){
          paste0("The R library ", pck.package, " was originally  written and is maintained by ", pck.author, ".\n\n")
        } else {
          paste0("The R library ", pck.package, " was originally written by ", pck.author, "\n",
          "and is maintained by ", pck.maintainer, ".\n\n")
        },
        "This Debian package was put together ", deb.description[["Maintainer"]], ".\n\n",
        if("Homepage" %in% deb.description){
          paste0("The sources were downloaded from ", deb.description[["Homepage"]], ".\n\n")
        } else {},
        "The package was renamed from its upstream name '", pck.package, "' to\n",
        "'", deb.pckg.name, "' in harmony with the R packaging policy to indicate\n",
        "that the package is external to the CRAN or BioC repositories.\n\n",
        includeLicense)

      # write the copyright file
      cat(deb.txt.copyright, file=deb.file.copyright)
      message("deb: debian/copyright updated.")
    } else {}

    ## debian/changelog
    ## TODO: rework this using dch from the debian devscripts package:
    # export DEBFULLNAME="<name>" ; export DEBEMAIL="name@example.com" ; dch --newversion="0.0X-XX-X" "<text>"
    # export DEBFULLNAME="<name>" ; export DEBEMAIL="name@example.com" ; dch --append "<text>"
    if(!file_test("-f", deb.file.changelog) | "changelog" %in% overwrite){
      deb.txt.changelog <- paste0(
        deb.srcs.name, " (", deb.pckg.vers, ") ", distribution, "; urgency=", urgency,"\n\n  * ",
        paste(changelog, collapse="\n  * "),
        "\n\n -- ", deb.description[["Maintainer"]], "  ", dateRFC2822(), "\n\n")

      # check if we need to write to the changelog at all
      if(file_test("-f", deb.file.changelog)){
        current.changelog <- readLines(deb.file.changelog)
        alreadyInLog <- any(grepl(paste0(deb.srcs.name, " \\(", deb.pckg.vers, "\\)"), current.changelog))
        if(isTRUE(alreadyInLog)){
          message(paste0("there's already a changelog entry for ", deb.srcs.name, " (", deb.pckg.vers, "), skipping!"))
        } else {
          deb.txt.changelog <- paste0(deb.txt.changelog, paste(current.changelog, collapse="\n"), "\n")
          # write the changelog file
          cat(deb.txt.changelog, file=deb.file.changelog)
        }
      } else {
        # write the changelog file
        cat(deb.txt.changelog, file=deb.file.changelog)
      }
      message("deb: debian/changelog updated.")
    } else {}

    ## debian/rules
    if(!file_test("-f", deb.file.rules) | "rules" %in% overwrite){
      deb.txt.rules <- paste0(
        "#!/usr/bin/make -f\n",
        "#\t\t\t\t\t\t\t\t-*- makefile -*-\n",
        "# debian/rules file for the Debian/GNU Linux ", deb.pckg.name," package\n",
        "# Copyright ", thisYear, " by ", deb.description[["Maintainer"]], "\n\n",
        "debRreposname := ", origin, "\n\n",
        "include /usr/share/R/debian/r-cran.mk\n")
#         "include /usr/share/R/debian/r-cran.mk\n\n",
#         "# Require a number equal or superior than the R version the package was built with.\n",
#         "install/$(package)::\n",
#         "\techo \"R:Depends=r-base-core (>= $(shell R --version | head -n1 | perl -ne 'print / +([0-9]\\.[0-9]+\\.[0-9])/')~)\" >> debian/$(package).substvars\n")

      # write the rules file
      cat(deb.txt.rules, file=deb.file.rules)
      # set executable permissions
      Sys.chmod(deb.file.rules, mode="0755")
      message("deb: debian/rules updated.")
    } else {}
  } else {}
  ## end "deb"

  if(any(c("bin","src") %in% actions)){
    old.wd <- getwd()
    on.exit(setwd(old.wd), add=TRUE)
    setwd(file.path(pck.source.dir, ".."))
    # copy the source files to build dir
    if(!identical(build.dir, pck.source.dir)){
      # unfortunately, file.copy() cannot exclude patterns, so we'll circumvent this by using tar
      tmp.tar.dest <- file.path(build.dir, "energize.tar")
      tar(tmp.tar.dest, files=pck.src.folder.name, tar=buildTools[["tar"]],
        compression="none", extra_flags="-h --exclude=*\\~ --exclude-vcs")
      setwd(build.dir)
      untar("energize.tar")
      stopifnot(file.remove("energize.tar"))
    } else {}
  } else {}

  if("src" %in% actions){
    prev.wd <- getwd()
    # first clean up
    setwd(file.path(build.dir, pck.src.folder.name))
    system("fakeroot debian/rules clean")
    setwd(file.path(build.dir))
    orig.file.name <- paste0(deb.srcs.name, "_", pck.version, ".orig.tar.gz")
    tar(orig.file.name, files=pck.src.folder.name, tar=buildTools[["tar"]],
      compression="gzip", extra_flags=paste0("-h --exclude=", pck.src.folder.name, "/debian --exclude=*\\~ --exclude-vcs"))
    system(paste0(buildTools[["dpkg-source"]], " -b ", pck.src.folder.name))
    src.files.to.move <- list.files(pattern="*.dsc$|*.debian.tar.gz$|*.orig.tar.gz$")
    file.copy(src.files.to.move, file.path(repo.src.real.path, src.files.to.move), overwrite=TRUE)
    message("deb: copied *.dsc, *.orig.tar.gz and *.debian.tar.gz files to debian source repository.")
    # update sources information; paths must be relative to the debian repo root
    setwd(file.path(repo.deb.path))
    dpkg.scans.call <- paste0(buildTools[["apt-ftparchive"]], " sources ", repo.src.real.rel.path, " > ", repo.src.pseudo.rel.path, "/Sources && \\\n",
    "cat ", repo.src.pseudo.rel.path, "/Sources | gzip -9 > ", repo.src.pseudo.rel.path, "/Sources.gz && \\\n",
    "cat ", repo.src.pseudo.rel.path, "/Sources | bzip2 -9 > ", repo.src.pseudo.rel.path, "/Sources.bz2")
    system(dpkg.scans.call, intern=TRUE)
    setwd(prev.wd)
   } else {}

  if("bin" %in% actions){
    prev.wd <- getwd()
    dpkg.build.call <- paste0(buildTools[["dpkg-buildpackage"]], " ", bin.opts)
    dpkg.gench.call <- paste0(buildTools[["dpkg-genchanges"]], " -b > ../", deb.pckg.name, "_", pck.version, "-", revision, "_", arch, ".changes")
    bin.build.dir <- file.path(build.dir, pck.src.folder.name)
    setwd(bin.build.dir)
    # work around probably buggy r-cran.mk by creating a symlink
    if(!identical(deb.pckg.name.lower, deb.pckg.name)){
      setwd(file.path(bin.build.dir, "debian"))
      system(paste0("ln -s ", deb.pckg.name, " ", deb.pckg.name.lower), intern=TRUE)
      setwd(bin.build.dir)
      message("deb: created workaround symlink for r-cran.mk.")
    } else {}
    system(dpkg.build.call, intern=TRUE)
    system(dpkg.gench.call, intern=TRUE)
    # copy built files
    setwd(file.path(build.dir))
    bin.files.to.move <- list.files(pattern="*.changes$|*.deb$")
    file.copy(bin.files.to.move, file.path(repo.bin.path, bin.files.to.move))
    message("deb: copied *.changes and *.deb files to debian binary repository.")
    # update package information; paths must be relative to the debian repo root
    setwd(file.path(repo.deb.path))
    dpkg.scanp.call <- paste0(buildTools[["apt-ftparchive"]], " packages ", repo.bin.rel.path, " > ", repo.bin.rel.path, "/Packages && \\\n",
    "cat ", repo.bin.rel.path, "/Packages | gzip -9 > ", repo.bin.rel.path, "/Packages.gz && \\\n",
    "cat ", repo.bin.rel.path, "/Packages | bzip2 -9 > ", repo.bin.rel.path, "/Packages.bz2")
    system(dpkg.scanp.call, intern=TRUE)
    for (this.path in c(repo.arch.paths)){
      repo.all.pckgs.files <- c("Packages", "Packages.gz", "Packages.bz2")
      file.copy(file.path(repo.bin.rel.path, repo.all.pckgs.files), file.path(this.path, repo.all.pckgs.files), overwrite=TRUE)
    }
    setwd(prev.wd)
  } else {}

  # update the a Release file
  if(any(c("bin","src") %in% actions)){
    prev.wd <- getwd()
    setwd(file.path(repo.deb.path))
    dpkg.relse.call <- paste0(buildTools[["apt-ftparchive"]],
      # Origin, Label, Suite, Version, Codename, Date, Valid-Until, Architectures, Components, Description
      "\\\n  -o=APT::FTPArchive::Release::Suite=\"", distribution, "\"",
      "\\\n  -o=APT::FTPArchive::Release::Components=\"", component, "\"",
      "\\\n  -o=APT::FTPArchive::Release::Architectures=\"i386 amd64 source\"",
      "\\\n  release ", repo.release.path, " > ", repo.release.path, "/Release")
    system(dpkg.relse.call, intern=TRUE)
    # sign release file
    if(!is.null(gpg.key)){
      # first check if the public key is already in the repository
      # this could later be replaced by generating a keyring.deb package
      if(!file_test("-f", repo.gpg.key.file) | "gpg.key" %in% overwrite){
        gpg.copy.call <- paste0(buildTools[["gpg"]], " --armor --output ", repo.gpg.key.file, " --export ", gpg.key)
        system(gpg.copy.call, intern=TRUE)
        message(paste0("deb: updated GnuPG key file in repository: ", gpg.key, ".asc"))
      } else {}

      # --no-tty --yes is mandatory, otherwise gpg will stop with an error
      # because it will try to get password information from /dev/tty and/or
      # ask if files should be re-signed
      gpg.sign.call <- paste0(buildTools[["gpg"]], " --no-tty --yes --default-key ", gpg.key, " -abs -o ", repo.release.path, "/Release.gpg ", repo.release.path, "/Release")
      system(gpg.sign.call, intern=TRUE)
      message(paste0("deb: signed Release file with key ", gpg.key, "."))
    } else {}
    setwd(prev.wd)
  } else {}

  if(isTRUE(keep.build)){
    message(paste0("deb: build files can be found under ", build.dir, "."))
    return(build.dir)
  } else {
    return(invisible(NULL))
  }
}
