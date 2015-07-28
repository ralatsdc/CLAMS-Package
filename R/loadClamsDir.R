#' @title Load a collection of data files
#'
#' @description
#' Loads a collection of CLAMS data files.
#'
#' @details
#' Loads each CLAMS data file contained in a directory.
#'
#' @param data.dir the directory containing CLAMS data files
#' 
#' @return
#' A CLAMS collection list containing a CLAMS data list for each CLAMS
#' data file. A CLAMS data list contains two elements:
#' \item{meta.data}{a list of meta data values}
#' \item{measurements}{a data frame of measurements}
#' 
#' @examples
#' ## Assign a CLAMS data directory
#' data.dir <- system.file("extdata", "Test", package="CLAMS")
#' clams.dir <- file.path(data.dir, "Collection-2013-01-15")
#'
#' ## Load a CLAMS data directory
#' clams.coll <- loadClamsDir(clams.dir)
#'
#' @export

loadClamsDir <- function(data.dir) {
  
  ## Copyright (c) 2014 Katherine B. and Raymond A. LeClair
  ## 
  ## This program can be redistributed and/or modified under the terms
  ## of the MIT License as published by the Open Source Initiative.
  ## 
  ## See the LICENSE file or http://opensource.org/licenses/MIT.
  
  ## Check length and mode of user provided input
  if (length(data.dir) != 1 || !is.character(data.dir) || data.dir == "") {
    stop("A single, character directory name is required")
  }

  ## Initialize return value
  clams.coll <- list()
  
  ## Identify data files
  data.files <- list()
  if (!file.exists(data.dir)) {
    stop(paste('Directory', data.dir, 'does not exist'))
  }
  files <- list.files(data.dir)
  for (file in files) {

    ## Skip non-data files
    if (   grepl("Description", file)
        || grepl("Rejected", file)
        || grepl("Extracted", file)
        || grepl('^#', file, perl=TRUE)
        || grepl('#$', file, perl=TRUE)
        || grepl('^~', file, perl=TRUE)
        || grepl('~$', file, perl=TRUE)) {
      next
    }

    ## Append data files
    if (grepl(".CSV$", file, perl=TRUE)) {
      data.files <- c(data.files, file)
    }
  }

  ## Consider each data file
  for (data.file in data.files) {

    ## Load the CLAMS data from the current file, and assign the
    ## resulting CLAMS data list
    clams.coll[[data.file]] <- loadClamsFile(paste(data.dir, data.file, sep="/"))

  }
  clams.coll
}
