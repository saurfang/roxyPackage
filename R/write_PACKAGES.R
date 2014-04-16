#' Generate PACKAGES files
#' 
#' Generate 'PACKAGES' and 'PACKAGES.gz' files for a repository of source or Mac/Windows binary packages. This directly calls \code{\link[tools]{write_PACKAGES}}.
#' 
#' In addition, it has the following enhancements:
#' \enumerate{
#'  \item Fetch files on Network Drive to tmp folder before producing INDEX
#' }
#' 
#' @param dir Character vector describing the location of the repository
#' (directory including source or binary packages) to generate the
#' 'PACKAGES' and 'PACKAGES.gz' files from and write them to.
#' @param type Type of packages: currently source ‘.tar.gz’ archives, 
#' and Mac or Windows binary (‘.tgz’ or ‘.zip’, respectively) packages are
#' supported. Defaults to "win.binary" on Windows and to "source" otherwise.
#' @param verbose logical. Should packages be listed as they are processed?
#' @param subdirs either logical (to indicate if subdirectories should be
#' included, recursively) or a character vector of name of subdirectories
#' to include.
#' @param latestOnly logical: if multiple versions of a package are available should only the latest version be included? (\code{\link{archive.packages}} only works for source packages when \code{latestOnly} is \code{FALSE})
#' @param ... additional arguments passed to \code{\link[tools]{write_PACKAGES}}
#' 
#' @importFrom tools write_PACKAGES
#' @seealso \code{\link[tools]{write_PACKAGES}}
#' @export
write_PACKAGES <- function(dir = ".", type = c("source", "mac.binary", "win.binary"),  verbose = FALSE, subdirs = FALSE, latestOnly = TRUE, ...){
  if (missing(type) && .Platform$OS.type == "windows") 
    type <- "win.binary"
  type <- match.arg(type)
  
  default_write_PACKAGES <- function(mydir=dir){
    tools::write_PACKAGES(mydir, type=type, verbose=verbose, subdirs=subdirs, latestOnly=latestOnly, ...)
  }
  
  if(.Platform$OS.type == "windows" && type == "source"){
    #expand to absolute path
    tryCatch({
      dir <- normalizePath(dir)
    }, warning = function(x){
      warning("Cannot normalize Path: ", dir, " because ", x)
      return(0)
    })
    
    #find drive letter
    idx <- grep(":", dir, fixed=TRUE)
    if(length(idx) == 0){
      if(verbose)
        message("can't find a driver letter in: ", dir)
      return(default_write_PACKAGES())
    }
    drive <- substring(dir, 0, idx)
    
    #see if this is a network drive
    isNetworkDrive <- FALSE
    tryCatch({
      shell(paste0("net use ", drive, ":"), intern=TRUE)
      if(verbose)
        message(drive, " is a network drive")
      isNetworkDrive <- TRUE
    }, warning = function(x){
      if(verbose)
        message(drive, " is not a network drive")
    })
    if(!isNetworkDrive)
      return(default_write_PACKAGES())
    
    #We now need to copy directory because untar is very expensive on network drive
    srcDir <- shortPathName(dir)
    tmpDir <- shortPathName(file.path(Sys.getenv("TMP"), paste0(".r_cache_", Sys.info()["user"], "_repo")))
    if(verbose)
      message("Establishing local copy of source repository...")
    # /D copy only updated files
    # /E Copy subdirectories and all files
    # /Y Suppress overwrite confirmation
    # /I Assume destination is a directory
    # /Z Enable restartable mode for network copies
    #TODO: What if one of the file is corrupt? hashing?
    shell(paste0("xcopy /D /E /Y /I /Z ", srcDir, " ", tmpDir))
    
    #Now temp folder might contains more files than source folder
    oldwd <- setwd(tmpDir)
    srcFiles <- list.files(srcDir, recursive=subdirs)
    tmpFiles <- list.files(tmpDir, recursive=subdirs)
    #Exclude directories as file.remove can cause trouble if not empty
    file.remove(setdiff(tmpFiles[!file.info(tmpFiles)$isdir], srcFiles))
    setwd(tmpDir)
    
    value <- default_write_PACKAGES(tmpDir)
    
    #Copy index files back
    if(verbose)
      message("Syncing index files back to source repository...")
    file.copy(file.path(tmpDir,"PACKAGES"), srcDir, overwrite = TRUE)
    file.copy(file.path(tmpDir,"PACKAGES.gz"),srcDir, overwrite = TRUE)
    
    return(invisible(value))
  }
  
  return(default_write_PACKAGES())
}