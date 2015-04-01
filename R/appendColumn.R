#' @title Appends a named column value in a time interval
#'
#' @description
#' Appends a named column value in a time interval to the measurements
#' data frame of a CLAMS data list or of each CLAMS data list in a
#' CLAMS collection list.
#'
#' @details
#' Translates the column name to upper case characters.
#'
#' Assignment in the time interval requires that the CLAMS measurement
#' data frame contain a date-time column. One is prepended, if
#' needed. The start and stop time strings are optional. If absent,
#' the start time will correspond to the start time of the
#' measurements, and stop time will correspond to the stop time of the
#' measurements.
#'
#' By default, the time interval does not recur daily. If
#' \code{is.daily} is \code{TRUE}, the start and stop time strings
#' must contain only time of day, and not date.
#'
#' @param clams.list a CLAMS data or collection list
#' @param col.name the column name
#' @param col.value the column value
#' @param start.str the start time string in format "\%I:\%M:\%S \%p"
#' or "\%m/\%d/\%Y \%I:\%M:\%S \%p"
#' @param stop.str the stop time string in format "\%I:\%M:\%S \%p" or
#' "\%m/\%d/\%Y \%I:\%M:\%S \%p"
#' @param is.daily flag to indicate whether the time interval recurs
#' daily, or not
#'
#' @return
#' A CLAMS data or collection list. A CLAMS collection list contains
#' multiple CLAMS data lists. Each CLAMS data list contains two
#' elements:
#' \item{meta.data}{a list of meta data values}
#' \item{measurements}{a data frame of measurements}
#'
#' @examples
#' ## Assign and load a CLAMS data directory, and select default columns
#' data.dir <- system.file("extdata", "Test", package="CLAMS")
#' clams.dir <- file.path(data.dir, "Collection-2013-01-15")
#' clams.coll <- loadClamsDir(clams.dir)
#' clams.coll <- selectColumns(clams.coll)
#'
#' ## Append a column indicating daily periods of illumination
#' clams.coll <- appendColumn(clams.coll, "light", TRUE,
#'                            start.str="06:00:00 AM", stop.str="06:00:00 PM", is.daily=TRUE)

appendColumn <- function(clams.list, col.name, col.value, start.str="", stop.str="", is.daily=FALSE) {

  ## Copyright (c) 2014 Katherine B. and Raymond A. LeClair
  ## 
  ## This program can be redistributed and/or modified under the terms
  ## of the MIT License as published by the Open Source Initiative.
  ## 
  ## See the LICENSE file or http://opensource.org/licenses/MIT.
  
  ## If the input CLAMS list is a data list, convert it to a
  ## collection list
  if (identical(names(clams.list), c("meta.data", "measurements"))) {
    is.data <- TRUE
    clams.list <- list(DATA=clams.list)
    
  } else {
    is.data <- FALSE
  }
  
  ## Check length and mode of user provided input
  if (length(col.name) != 1 || !is.character(col.name)) {
    stop("A single, character column name is required")
  }
  if (length(col.value) != 1) {
    stop("A single column value is required")
  }
  if (start.str != "" &&
      (length(start.str) != 1 || !is.character(start.str))) {
    stop("A single start string is required")
  }
  if (stop.str != "" &&
      (length(stop.str) != 1 || !is.character(stop.str))) {
    stop("A single stop string is required")
  }
  if (length(is.daily) != 1 || !is.logical(is.daily)) {
    stop("A single, logical daily flag is required")
  }
  
  ## Consider each data list in the collection list
  for (i.list in seq(1, length(clams.list))) {
    clams.data <- clams.list[[i.list]]
    
    ## Prepend a date-time object, if one does not exist
    fmt.d <- "%m/%d/%Y"
    
    fmt.t <- "%I:%M:%S %p"
    fmt.d.t <- paste(fmt.d, fmt.t)
    if (!("D.T" %in% names(clams.data$measurements))) {
      clams.data$measurements <- cbind(
        list(D.T=as.POSIXct(strptime(clams.data$measurements$DATE.TIME, fmt.d.t))),
        clams.data$measurements)
    }
    clams.d.t <- clams.data$measurements$D.T
    
    ## Convert start time string to a date-time object
    clams.d.str <- strftime(clams.d.t[1], fmt.d)
    is.start.tod <- FALSE
    if (start.str == "") {
      
      ## The start time string is empty, so use the measurement start
      ## time
      start.d.t <- clams.d.t[1]
      
    } else if (!is.na(strptime(start.str, fmt.t))) {
      
      ## The start time string is a time of day, so use the
      ## measurement start date
      start.d.t <- as.POSIXct(strptime(paste(clams.d.str, start.str), fmt.d.t))
      is.start.tod <- TRUE
      
    } else if (!is.na(strptime(start.str, fmt.d.t))) {
      
      ## The start time string contains date and time of day, so use
      ## it
      start.d.t <- as.POSIXct(strptime(start.str, fmt.d.t))
      
    } else {
      stop("Invalid start time string")
    }
    
    ## Convert stop time string to a date-time object
    is.stop.tod <- FALSE
    if (stop.str == "") {
      
      ## The stop time string is empty, so use the measurement stop
      ## time
      stop.d.t <- clams.d.t[length(clams.d.t)]
      
    } else if (!is.na(strptime(stop.str, fmt.t))) {
      
      ## The stop time string is a time of day, so use the measurement
      ## _start_ date
      stop.d.t <- as.POSIXct(strptime(paste(clams.d.str, stop.str), fmt.d.t))
      is.stop.tod <- TRUE
      
      ## Assume stop time of day earlier than start time of day occurs
      ## on the following day
      if (stop.d.t < start.d.t) {
        
        ## Add a day's worth of seconds
        stop.d.t <- stop.d.t + 86400
        
      }
    } else if (!is.na(strptime(stop.str, fmt.d.t))) {
      
      ## The stop time string contains date and time of day, so use it
      stop.d.t <- as.POSIXct(strptime(stop.str, fmt.d.t))
      
      ## Ensure the start time is earlier than the stop time
      if (stop.d.t <= start.d.t) {
        stop("The start time must be earlier than the stop time")
      }
      
    } else {
      stop("Invalid stop time string")
    }
    
    ## If the time interval recurs daily, ensure the start and stop
    ## times are time of day
    if (is.daily && (!is.start.tod || !is.stop.tod)) {
      stop("The start and stop times must be time of day, if daily")
    }
    
    ## Initialize and append column, if NULL
    col.name <- toupper(col.name)
    if (is.null(clams.data$measurements[[col.name]])) {
      clams.data$measurements[[col.name]] <- vector(mode=typeof(col.value), length(clams.d.t))
    }
    
    ## Assign input value in indicated date-time intervals
    clams.data$measurements[[col.name]][start.d.t <= clams.d.t & clams.d.t < stop.d.t] <- col.value
    while (is.daily && stop.d.t < clams.d.t[length(clams.d.t)]) {
      
      ## Add a day's worth of seconds, and repeat assignment
      start.d.t <- start.d.t + 86400 
      stop.d.t <- stop.d.t + 86400
      clams.data$measurements[[col.name]][start.d.t <= clams.d.t & clams.d.t < stop.d.t] <- col.value
      
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
