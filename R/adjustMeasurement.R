#' @title Adjusts measurement columns
#'
#' @description
#' Adjusts a measurement column from the measurements data frame of a
#' CLAMS data list or each CLAMS data list in a CLAMS collection list
#' using weight for normalization.
#'
#' @details
#' Each measurement data frame must contain an "ORIG.WEIGHT" and an
#' "ADJ.WEIGHT" column. The adjusted measurement is appended to the
#' data frame with name prefix "ADJ.".
#' 
#' @param clams.list a CLAMS data or collection list
#' @param msr.name the measurement column name
#'
#' @return
#' A CLAMS data or collection list. A CLAMS collection list contains
#' multiple CLAMS data lists. Each CLAMS data list contains two
#' elements:
#' \item{meta.data}{a list of meta data values}
#' \item{measurements}{a data frame of measurements}
#'
#' @examples
#' ## Assign and load a CLAMS data directory
#' data.dir <- system.file("extdata", "Test", package="CLAMS")
#' clams.dir <- file.path(data.dir, "Collection-2015-01-06")
#' clams.coll <- loadClamsDir(clams.dir)
#'
#' ## Select columns and remove outliers
#' clams.coll <- selectColumns(clams.coll, do.remove.outliers=TRUE)
#'
#' ## Append original weight
#' clams.coll <- appendColumn(clams.coll, "orig.weight", c(23.8, 25.8, 22.6, 25.5, 25.5))
#'
#' ## Append adjusted weight in two intervals
#' clams.coll <- appendColumn(clams.coll, "adj.weight", c(23.5, 24.3, 22.6, 22.4, 24.9),
#'                            start.str="01/08/2015 06:00:00 AM", stop.str="01/09/2015 06:00:00 AM")
#' clams.coll <- appendColumn(clams.coll, "adj.weight", c(22.7, 24.2, 22.6, 25.1, 24.3),
#'                            start.str="01/11/2015 06:00:00 AM", stop.str="01/12/2015 06:00:00 AM")
#'
#' ## Adjust VO2 measurements
#' clams.coll <- adjustMeasurement(clams.coll, "VO2")

adjustMeasurement <- function(clams.list, msr.name) {

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
  if (length(msr.name) != 1 || !is.character(msr.name)) {
    stop("A single, character measurement name is required")
  }
  if (!(msr.name %in% c("VO2", "VCO2"))) {
    stop("Only 'VO2' and 'VCO2' measurement columns can be adjusted")
  }
  
  ## If the input CLAMS list is a data list, convert it to a
  ## collection list
  if (identical(names(clams.list), c("meta.data", "measurements"))) {
    is.data <- TRUE
    clams.list <- list(DATA=clams.list)

  } else {
    is.data <- FALSE
  }
  
  ## Consider each CLAMS data list in order use weight for
  ## normalization
  adj.name <- paste("ADJ", toupper(msr.name), sep=".")
  for (i.list in seq(1, length(clams.list))) {
    clams.data <- clams.list[[i.list]]
    if (!identical(names(clams.data), c("meta.data", "measurements"))) {
      stop("A CLAMS data or collection list is required")
    }
    
    ## Check needed measurement columns are present
    if (!(msr.name %in% names(clams.data$measurements))) {
      stop("The measurement column must be present in the measurements data frame of each CLAMS data list")
    }
    if (!("ORIG.WEIGHT" %in% names(clams.data$measurements))) {
      stop("An 'ORIG.WEIGHT' column must be present in the measurements data frame of each CLAMS data list")
    }
    if (!("ADJ.WEIGHT" %in% names(clams.data$measurements))) {
      stop("An 'ADJ.WEIGHT' column must be present in the measurements data frame of each CLAMS data list")
    }

    # Use weight for normalization
    clams.data$measurements[[adj.name]] <-
      clams.data$measurements[[msr.name]] /
        clams.data$measurements$ORIG.WEIGHT *
          clams.data$measurements$ADJ.WEIGHT

    ## Eliminate Infs
    clams.data$measurements[[adj.name]][clams.data$measurements[[adj.name]] == Inf] <- NA

    clams.list[[i.list]] <- clams.data
  }

  ## If the input CLAMS list is a data list, convert the corresponding
  ## collection list to a data list
  if (is.data) {
    clams.list <- clams.list$DATA
  }
  
  ## Return the modified CLAMS list
  clams.list
}
