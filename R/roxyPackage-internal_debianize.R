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


## function debianPkgName()
# origin: string or vector
debianPkgName <- function(package, origin=NULL, version=NULL, replace.dots=FALSE){
    package <- tolower(package)
    if(isTRUE(replace.dots)){
      package <- gsub("\\.", "-", package)
    } else {}

    if(is.null(version)){
      result <- package
    } else {
      result <- paste(package, version, sep=" ")
    }

    if(!is.null(origin)){
      result <- paste("r", origin, result, sep="-", collapse=" | ")
    } else {}

    return(result)
} ## end function debianPkgName()


## function splitDepends()
# takes a dependency vector (e.g., "foo (>> 3.0), bar, baz") and splits it into a matrix with
# one column for each package name and one for a version number, if present
# returns NULL if there are no dependencies
splitDepends <- function(dep){
  # just a precaution -- replace NA values
  dep[is.na(dep)] <- ""
  # remove names, trim & split
  dep <- trim(unlist(strsplit(as.character(dep), ",")))
  dep.length <- length(dep)
  if(dep.length > 0){
    results <- matrix(data="", nrow=dep.length, ncol=2, dimnames=list(NULL,c("package", "version")))
    for (thisDep.num in 1:dep.length){
      thisDep <- dep[thisDep.num]
      # split into <name> and (<version>)
      depParts <- unlist(strsplit(thisDep, split="[[:space:]]*\\("))
      results[thisDep.num,"package"] <- depParts[1]
      if(!is.na(depParts[2])){
        results[thisDep.num,"version"] <- paste0("(", depParts[2])
      } else {}
    }
  } else {
    results <- NULL
  }
  return(results)
}
## end function splitDepends()


## function debianizeDepends()
# origin: string or vector
# origin.alt: *named* list(pckgname=c("origin.string"))
# R: the package name for R, usually not "r-cran-<something>". if set to NULL, R will be dropped completely if present
# forceRVersion: omitted if NULL, i.e., the R version string is used as defined in DESCRIPTION (if any)
# collapse: if NULL returns a named vector (names are the original R package names, values the debian package names)
# drop.version: if TRUE returns only the package names (overwritten by forceRVersion!)
# append: a character string/vector to append additional debian package names to the results
debianizeDepends <- function(dep, origin="cran", origin.alt=list(), R="r-base-core",
  base=c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
    "parallel", "splines", "stats", "stats4", "tools", "tcltk", "utils"),
  forceRVersion=getRvers(), collapse=", ", drop.version=FALSE, append=NULL, replace.dots=FALSE){

  dep.split <- splitDepends(dep)
  if(is.null(dep.split)){
    return("")
  } else {}
  names(origin.alt) <- tolower(names(origin.alt))
  other.origin.names <- names(origin.alt)

  base.packages <- tolower(base)

  dep.list <- list()
  # minimum requirements?
  if(!is.null(forceRVersion) && !is.null(R)){
    dep.list[["R"]] <- list(package=R, version=paste0("(>= ", forceRVersion, ")"), origin=NULL)
  } else {}

  for (thisDep in 1:nrow(dep.split)){
    dep.lst.key <- dep.split[thisDep, "package"]
    dep.lst.package <- tolower(dep.lst.key)

    # check for special cases:
    # - packages from base should be dropped
    if(dep.lst.package %in% base.packages){
      next
    } else {}

    if(nchar(dep.split[thisDep, "version"]) > 0 && !isTRUE(drop.version)){
      dep.lst.version <- dep.split[thisDep, "version"]
    } else {
      dep.lst.version <- NULL
    }

    # - R should be translated into the value of "R", or be dropped?
    if(dep.lst.package == "r"){
      if(!is.null(forceRVersion) || is.null(R)){
        next
      } else {
        dep.lst.package <- R
        dep.list[["R"]] <- list(package=R, version=dep.lst.version, origin=NULL)
        next
      }
    } else {}

    dep.lst.origin <- origin
    # check if this package should come from another origin
    if(dep.lst.package %in% other.origin.names){
      dep.lst.origin <- origin.alt[[dep.lst.package]]
    } else {}
    dep.list[[dep.lst.key]] <- list(package=dep.lst.package, version=dep.lst.version, origin=dep.lst.origin)
  }
  
  # re-combine to <origin>-<name> (<version>)
  results <- sapply(dep.list, function(thisDep){
      debianPkgName(package=thisDep[["package"]], origin=thisDep[["origin"]], version=thisDep[["version"]], replace.dots=replace.dots)
    })

  if(!is.null(append)){
    results <- c(results, append)
  } else {}

  if(is.null(collapse)){
    return(results)
  } else {
    return(paste(results, collapse=collapse))
  }
} ## end function debianizeDepends()


## function deb.check.sources()
# checks if we're dealing with a sources directory, a tarball or need to download something
deb.check.sources <- function(src,
  dl.dir=file.path(tempdir(),"roxyPackge","downloads"),
  local.src.dir=file.path(tempdir(),"roxyPackge","local_sources")){

  if(file_test("-d", src)){
    return(src)
  } else if(grepl("^http://|^https://|^ftp://|^sftp://", src, ignore.case=TRUE)){
    # seems to be a URL for downloading
    short.file.name <- gsub("(.*/)([^/]*)", "\\2", src, perl=TRUE)
    message(paste0("deb: preparing for download ", short.file.name,  "..."))
    createMissingDir(dirPath=dl.dir, action="deb")
    dl.local.path <- file.path(dl.dir, short.file.name)
    download.file(url=src, destfile=dl.local.path)
    message(paste("deb: downloaded package to", dl.local.path))
    full.path <- dl.local.path
  } else if(grepl("\\.gz$|\\.tgz$", src, ignore.case=TRUE)){
    # seems to be a local tarfile
    short.file.name <- gsub("(.*/)([^/]*)", "\\2", src, perl=TRUE)
    full.path <- normalizePath(src)
  } else {
    stop(simpleError(paste0("deb: unable to deal with ", paste0(src, collapse=", "))))
  }
  createMissingDir(dirPath=local.src.dir, action="deb")
  message(paste0("deb: unpacking ", short.file.name,  "..."))
  # now we need to know the dirname which will be created upon untaring
  # we'll assume we get exactly one directory -- if not, stop with an error
  # to be on the safe side...
  all.files <- untar(full.path, list=TRUE)
  untar.dir <- unique(gsub("([^/]*)(.*)", "\\1", all.files, perl=TRUE))
  if(length(untar.dir) > 1 || nchar(untar.dir) < 1){
    stop(simpleError("deb: sorry, couldn't determine a sane directory name from the archive!"))
  } else {}
  untar(full.path, exdir=local.src.dir)
  return(file.path(local.src.dir, untar.dir))
} ## end function deb.check.sources()


## function check.installed.deps()
# tries to find out whether a needed dependency is already installed
# returns either TRUE/FALSE, or the package name(s) found
# dep: can be a vector, but they will be treated as *alternatives*, that is, result will be TRUE if
#   any one of the packages is found (as an alternative to do "package1 | package2" checks)
check.installed.deps <- function(dep, value=FALSE){
  if(!isUNIX()){
    stop(simpleError("this doesn't seem to be a UNIX system, so i assume it's not Debain as well!"))
  } else {}

  dpkg <- Sys.which("dpkg")
  if("" %in% dpkg){
    stop(simpleError("can't find dpkg -- are you sure this is a Debain system?!"))
  } else {}

  get.installed.syscall <- paste0(dpkg, " --get-selections")
  all.installed <- system(get.installed.syscall, intern=TRUE)

  # look for the dependency in all installed packages
  whatWasFound <- grep(paste0("^(", paste0(gsub("[[:space:]]", "", dep), collapse="|"), ")[[:space:]]*install"), all.installed, value=TRUE)
  if(isTRUE(value)){
    return(gsub("[[:space:]]*install", "", whatWasFound))
  } else {
    if(length(whatWasFound) > 0){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
} ## end function check.installed.deps()


## function check.deb.availability()
# checks if a given debian package is available in the configured repositories
# the debian package name can be generated from the R package name
check.deb.availability <- function(deb=NULL, R=NULL, origin="cran", replace.dots=FALSE){
  if(!isUNIX()){
    stop(simpleError("this doesn't seem to be a UNIX system, so i assume it's not Debain as well!"))
  } else {}

  if(is.null(deb)){
    stopifnot(!is.null(R))
    deb <- debianPkgName(package=R, origin=origin, version=NULL, replace.dots=replace.dots)
  } else {}

  aptCache <- Sys.which("apt-cache")
  if("" %in% aptCache){
    stop(simpleError("can't find apt-cache -- are you sure you've set up everything?"))
  } else {}

  # look for the package with a regexp
  get.avails.syscall <- paste0(aptCache, " search ^", deb, "$")
  all.avails <- system(get.avails.syscall, intern=TRUE)

  if(length(all.avails) > 0){
    return(TRUE)
  } else {
    return(FALSE)
  }
} ## end function check.deb.availability()


## function dl.missing.dep.src()
# a function to download R source packages of missing dependencies
# deps: character vector of R package names
# subdir/order: character and numeric, can be used to sort the downloads indow several subdirectories;
#   especially helpful if recursive=TRUE, so you know which packages must be dealt with in what order (highest numbers first)
# all: if TRUE, attempts to download all sources (even for packages which are not missing)
# previous: previous downloads by parent calls, to check if some packages are already downloaded (used in recursive loops)
dl.missing.dep.src <- function(deps, destdir=file.path(tempdir(),"roxyPackge","downloads"), subdir=deps[1], order=NULL, recursive=FALSE,
  repos=getOption("repos"), all=FALSE, check.deb=TRUE, origin="cran", origin.alt=list(), previous=list(), available=NULL){

  # first of all, check for a debian package, if desired, because if there is one, the rest is obsolete
  deb.packages <- previous[["deb"]]
  if(isTRUE(check.deb)){
    other.origin.names <- names(origin.alt)
    for (thisDep in deps){
      dep.lst.origin <- origin
      # check if this package should come from another origin
      if(thisDep %in% other.origin.names){
        dep.lst.origin <- origin.alt[[thisDep]]
      } else {}
      for (thisOrigin in dep.lst.origin){
        thisDebPackage <- debianPkgName(package=thisDep, origin=thisOrigin)
        is.available <- check.deb.availability(deb=thisDebPackage)
        if(isTRUE(is.available)){
          # ok, there is a debian package, add it to the list
          deb.packages <- c(deb.packages, thisDebPackage)
          names(deb.packages)[length(deb.packages)] <- thisDep
          # remove the dependency from the ones to check
          deps <- deps[!deps %in% thisDep]
          break
        } else {}
      }
    }
  } else {}
  # are we done already?
  if(length(deps) == 0 || isTRUE(nchar(deps) == 0)){
    return(list(order=order, dl.result=previous[["dl"]], deb=deb.packages))
  } else {}

  destdir.first <- destdir
  if(!is.null(subdir)){
    destdir <- file.path(destdir, subdir)
  } else {}
  if(is.numeric(order)){
    destdir <- file.path(destdir, sprintf("%03.f", order))
  } else {}

  createMissingDir(dirPath=destdir, action="deb")

  # create result and sub-result objects, appended to previous results if present
  if(is.null(available)){
    available <- available.packages(contriburl=contrib.url(repos))
  } else {}
  # check if the package is known
  unknown.packages <- !deps %in% available[,"Package"]
  if(any(unknown.packages)){
    warning(paste0("dep: some R packages cannot be found in the configured repositories and were skipped, please check:\n  ",
      paste0(deps[unknown.packages], collapse=", ")))
    deps <- deps[!unknown.packages]
  } else {}
  dl.result <- download.packages(pkgs=deps, destdir=destdir, available=available, repos=repos)
  final.results <- rbind(previous[["dl"]], dl.result)
  if(isTRUE(recursive)){
    for (thisSrc in 1:nrow(dl.result)){
      thisPackageName <- dl.result[thisSrc,1]
      thisPackageLocation <- dl.result[thisSrc,2]
      DESC.temp <- tempfile(paste0(thisPackageName, "DESCRIPTION"), tmpdir=destdir)
      createMissingDir(dirPath=DESC.temp, action="deb", quiet=TRUE)
      on.exit(unlink(DESC.temp, recursive=TRUE))
      # look at the DESCRIPTION of each downloaded package and check Depends/Imports
      untar(thisPackageLocation, files=file.path(thisPackageName, "DESCRIPTION"), exdir=DESC.temp)
      thisDepends <- read.dcf(file.path(DESC.temp, thisPackageName, "DESCRIPTION"), fields=c("Depends","Imports"))
      thisDepends <- as.character(thisDepends[!is.na(thisDepends)])
      # debianize the names, dropping R
      this.debDeps <- debianizeDepends(thisDepends, R=NULL, collapse=NULL, drop.version=TRUE, origin=origin, origin.alt=origin.alt)
      if(length(this.debDeps) > 0 && nchar(this.debDeps) > 0){
        # run check.installed.deps()
        for (this.debDeps.num in 1:length(this.debDeps)){
          this.debDeps.R <- names(this.debDeps)[this.debDeps.num]
          this.debDeps.deb <- this.debDeps[this.debDeps.num]
          if(isTRUE(all)){
            # we shall get all sources, so this will always be FALSE no matter what
            chk.result <- FALSE
          } else {
            chk.result <- check.installed.deps(this.debDeps.deb)
          }
          # if missing, run recursively to fetch the sources
          if(!isTRUE(chk.result)){
            message(paste0(thisPackageName, " depends on ", this.debDeps.deb, "..."))
            # see if we should prefer existing debian packages
            if(isTRUE(check.deb)){
              is.available <- check.deb.availability(deb=this.debDeps.deb)
              if(isTRUE(is.available)){
                # ok, there is a debian package, add it to the list
                deb.packages <- c(deb.packages, this.debDeps.deb)
                next
              } else {}
            } else {}
            # increase "order" if not NULL
            if(is.numeric(order)){
              order <- order + 1
            } else {}
            # check if this dependency has already been downloaded
            if(!this.debDeps.R %in% final.results[,1]){
              sub.result <- dl.missing.dep.src(deps=this.debDeps.R, destdir=destdir.first, subdir=subdir, order=order,
                recursive=recursive, repos=repos, all=all, check.deb=check.deb, origin=origin, origin.alt=origin.alt,
                previous=list(dl=final.results, deb=deb.packages), available=available)
              order <- sub.result[["order"]]
              deb.packages <- sub.result[["deb"]]
              final.results <- sub.result[["dl.result"]]
            } else {
              # if we're in "order" mode, rename the download folder to the current number
              # other wise we're fine already
              if(is.numeric(order)){
                current.entry <- final.results[final.results[,1] %in% this.debDeps.R,2]
                entry.file.name <- basename(current.entry)
                entry.dir.old <- dirname(current.entry)
                entry.dir.new <- file.path(dirname(entry.dir.old), sprintf("%03.f", order))
                # just a precaution, make sure we only try to rename stuff in the very download folder
                if(isTRUE(grepl(destdir.first, entry.dir.old))){
                  rename.success <- file.rename(from=entry.dir.old, to=entry.dir.new)
                  if(isTRUE(rename.success)){
                    final.results[final.results[,1] %in% this.debDeps.R,2] <- file.path(entry.dir.new, entry.file.name)
                  }
                } else {}
              } else {}
            }
          } else {}
        }
      } else {}
    }
  } else {}
  if(!is.null(order)){
    # bring entries into order
    final.results <- final.results[order(final.results[,2]),]
  } else {}
  results <- list(order=order, dl.result=final.results, deb=deb.packages)
  return(results)
} ## end function dl.missing.dep.src()


## function check.append()
# checks if there's an "append" element in an object and returns either that or NULL (or TRUE/FALSE)
check.append <- function(dep, check="append", value=FALSE){
  if(isTRUE(check %in% names(dep[1]))){
    if(isTRUE(value)){
      result <- dep[1][[check]]
    } else {
      result <- TRUE
    }
  } else {
    if(isTRUE(value)){
      result <- NULL
    } else {
      result <- FALSE
    }
  }
  return(result)
} ## end function check.append()
