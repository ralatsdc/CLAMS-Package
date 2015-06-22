#' @title Selects named columns
#'
#' @description
#' Selects named columns from the measurements data frame of a
#' CLAMS data list or of each CLAMS data list in a CLAMS collection
#' list. Optionally, removes outliers.
#'
#' @details
#' The vector of measurement column names is optional. If absent, the
#' default selection of columns includes: "INTERVAL", "CHAN",
#' "DATE.TIME", "VO2", "VCO2", "HEAT", "RER", "FEED1.ACC", "XTOT",
#' "XAMB", and "BODY.TEMP".
#'
#' By default outliers are not removed. If \code{do.remove.outliers}
#' is \code{TRUE}, outliers, including non-physical values, and values
#' greater than three standard deviations from the mean, are
#' removed. Non-physical values include values of VO2, VCO2, and HEAT
#' less than or equal to zero, RER less than 0.5 or more than 1.2, and
#' XTOT and XAMB less than zero.
#'
#' @param clams.list a CLAMS data or collection list
#' @param col.names the column names to select
#' @param do.remove.outliers flag to remove outliers, or not
#'
#' @return
#' A CLAMS data or collection list. A CLAMS collection list contains
#' multiple CLAMS data lists. Each CLAMS data list contains two
#' elements:
#' \item{meta.data}{a list of meta data values}
#' \item{measurements}{a data frame of measurements}
#'
#' @examples
#' ## Assign and load a CLAMS data file
#' data.dir <- system.file("extdata", "Test", package="CLAMS")
#' clams.file <- file.path(data.dir, "Collection-2013-01-15", "2013-01-15.0101.CSV")
#' clams.data <- loadClamsFile(clams.file)
#' 
#' ## Select default columns
#' clams.data <- selectColumns(clams.data)
#' 
#' ## Assign and load a CLAMS data directory
#' clams.dir <- file.path(data.dir, "Collection-2013-01-15")
#' clams.coll <- loadClamsDir(clams.dir)
#'
#' ## Select default columns, a single column, and a single column with
#' ## outliers removed
#' clams.coll <- selectColumns(clams.coll)
#' clams.coll <- selectColumns(clams.coll, col.names=c("VO2"))
#' clams.coll <- selectColumns(clams.coll, col.names=c("VO2"), do.remove.outliers=TRUE)

selectColumns <- function(clams.list, col.names=c(), do.remove.outliers=FALSE) {

  ## Copyright (c) 2014 Katherine B. and Raymond A. LeClair
  ## 
  ## This program can be redistributed and/or modified under the terms
  ## of the MIT License as published by the Open Source Initiative.
  ## 
  ## See the LICENSE file or http://opensource.org/licenses/MIT.

  ## Check length and mode of user provided input
  if (!is.list(clams.list)) {
    stop("A CLAMS data or collection list is required")
  }
  if (!is.null(col.names) && !is.vector(col.names, mode="character")) {
    stop("Column names must be a character vector")
  }
  if (length(do.remove.outliers) > 1 || !is.logical(do.remove.outliers)) {
    stop("The remove outliers flag must have mode 'logical'")
  }

  ## If the input CLAMS list is a data list, convert it to a
  ## collection list
  if (identical(names(clams.list), c("meta.data", "measurements"))) {
    is.data <- TRUE
    clams.list <- list(DATA=clams.list)

  } else {
    is.data <- FALSE
  }

  ## If the column names argument is NULL, assign a default vector of
  ## names to use in selecting columns
  if (is.null(col.names)) {
    col.names = c(
      "INTERVAL", "CHAN", "DATE.TIME", "VO2", "VCO2", "HEAT", "RER",
      "FEED1.ACC", "XTOT", "XAMB", "BODY.TEMP")
  }

  ## Consider each data list in the collection list
  for (i.list in seq(1, length(clams.list))) {
    clams.data <- clams.list[[i.list]]
    if (!identical(names(clams.data), c("meta.data", "measurements"))) {
      stop("A CLAMS data or collection list is required")
    }
    
    ## Select the named columns
    clams.data$measurements <- clams.data$measurements[col.names]

    ## Remove outliers, if requested
    if (do.remove.outliers) {

      msrs <- intersect(c("VO2", "VCO2", "HEAT", "RER", "XTOT", "XAMB"), col.names)
      for (msr in msrs) {

        ## Ensure 0 < VO2, VCO2, HEAT
        if (is.element(msr, c("VO2", "VCO2", "HEAT"))) {
          clams.data$measurements[[msr]][clams.data$measurements[[msr]] <= 0] <- NA
        }

        ## Ensure 0.5 <= RER <= 1.2
        if (msr == "RER") {
          clams.data$measurements[[msr]][clams.data$measurements[[msr]] < 0.5] <- NA
          clams.data$measurements[[msr]][1.2 < clams.data$measurements[[msr]]] <- NA
        }

        ## Ensure 0 <= XTOT, XAMB
        if (is.element(msr, c("XTOT", "XAMB"))) {
          clams.data$measurements[[msr]][clams.data$measurements[[msr]] < 0] <- NA
        }

        ## Remove outliers
        mn.msr <- mean(clams.data$measurements[[msr]], na.rm=TRUE)
        sd.msr <- sd(clams.data$measurements[[msr]], na.rm=TRUE)
        clams.data$measurements[[msr]][clams.data$measurements[[msr]] < mn.msr - 3 * sd.msr] <- NA
        clams.data$measurements[[msr]][mn.msr + 3 * sd.msr < clams.data$measurements[[msr]]] <- NA
      }
    }

    ## Assign modified CLAMS data list
    clams.list[[i.list]] <- clams.data;
  }

  ## If the input CLAMS list is a data list, convert the corresponding
  ## collection list to a data list
  if (is.data) {
    clams.list <- clams.list$DATA
  }

  ## Return the modified CLAMS list
  clams.list
}
