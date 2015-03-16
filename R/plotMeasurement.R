#' @title Plots aligned measurement columns
#'
#' @description
#' Plots aligned measurement columns from the measurements data frame
#' of each CLAMS data list in a CLAMS collection list as a function of
#' a common time sequence in seconds.
#'
#' @details
#' Places PDF commands needed for the plot in output.file.
#' 
#' Aggregates named measurement columns as labeled columns using an
#' aggregation function, by default using \code{mean}. Optionally
#' plots and labels named test conditions. Note that measurement
#' column and test condition labels may be character or expression
#' vectors.
#' 
#' Optionally filters the measurements by applying a trailing one hour
#' average. By default, measurements are filtered. Optionally plots
#' error bars. By default, error bars are not plotted.
#'
#' @param clams.msr aligned measurement columns
#' @param msr.label measurement column labels
#' @param output.file file to contain plot output
#' @param agg.names aggregration column names
#' @param agg.labels aggregration column labels
#' @param agg.function aggregration function
#' @param con.names test condition column names
#' @param con.labels test condition plot labels
#' @param do.filter flag to filter measurements, or not
#' @param do.error.bars flag to plot error bars, or not
#'
#' @return
#' A boolean indicating success, or not.
#'
#' @examples
#' \dontrun{
#' plotMeasurement(clams.msr, "Oxygen Consumption, ml/kg/hr", "VO2.pdf",
#'                 agg.names=list(c("WT1", "WT2"), c("KO1", "KO2")),
#'                 agg.labels=c("WT", "KO"),
#'                 con.names=c("TEMP.23", "TEMP.04"),
#'                 con.labels=expression(paste("23", degree, "C"), paste("4", degree, "C")),
#'                 do.filter=TRUE, do.error.bars=TRUE)
#' plotMeasurement(clams.msr, "Oxygen Consumption, ml/kg/hr", "VO2.pdf",
#'                 agg.names=list(c("WT1", "WT2"), c("KO1", "KO2")),
#'                 agg.labels=c("WT", "KO"), agg.function=sum,
#'                 con.names=c("TEMP.23", "TEMP.04"),
#'                 con.labels=expression(paste("23", degree, "C"), paste("4", degree, "C")),
#'                 do.filter=TRUE, do.error.bars=TRUE)
#' }
plotMeasurement <- function(clams.msr, msr.label, output.file, agg.names, agg.labels, agg.function=mean,
                            con.names=NULL, con.labels=NULL, do.filter=TRUE, do.error.bars=FALSE) {
  ## Copyright (c) 2014 Katherine B. and Raymond A. LeClair
  ## 
  ## This program can be redistributed and/or modified under the terms
  ## of the MIT License as published by the Open Source Initiative.
  ## 
  ## See the LICENSE file or http://opensource.org/licenses/MIT.

  ## Initialize return value
  status <- FALSE

  ## Check length and mode of user provided input
  if (!(is.character(msr.label) || is.expression(msr.label))) {
    stop("The measurment label must be mode 'character' or 'expression'")
  }
  if (length(output.file) > 1 || !is.character(output.file)) {
    stop("The output file must be mode 'character'")
  }
  if (is.null(agg.names) || is.null(agg.labels)) {
    stop("Aggregation names and labels must be specified together")
  }
  if (!is.list(agg.names)) {
    stop("Aggregation names must be a list of character vectors")
  } else {
    for (names in agg.names) {
      if (!is.vector(names, mode="character")) {
        stop("Aggregation names must be a list of character vectors")
      }
    }
  }
  if (!(is.vector(agg.labels, mode="character") || is.vector(agg.labels, mode="expression"))) {
    stop("Aggregation labels must be character or expression vectors")
  }
  if (length(agg.names) != length(agg.labels)) {
    stop("Aggregation names and labels must have equal length")
  }
  if (length(agg.function) > 1 || !is.function(agg.function)) {
    stop("The aggregation function must be a function")
  }
  if (!is.null(con.names)) {
    if (is.null(con.labels)) {
      stop("Condition names and labels must be specified together")
    }
    if (!is.vector(con.names, mode="character")) {
      stop("Condition names must be character vectors")
    }
  }
  if (!is.null(con.labels)) {
    if (is.null(con.names)) {
      stop("Condition names and labels must be specified together")
    }
    if (!(is.vector(con.labels, mode="character") || is.vector(con.labels, mode="expression"))) {
      stop("Condition labels must be character or expression vectors")
    }
  }
  if (length(do.filter) > 1 || !is.logical(do.filter)) {
    stop("The filter flag must have mode 'logical'")
  }
  if (length(do.error.bars) > 1 || !is.logical(do.error.bars)) {
    stop("The error bar flag must have mode 'logical'")
  }

  ## Filter, if requested, then aggregate aligned measurement columns
  clams.sem <- list()
  d.t <- clams.msr[["D.T"]]
  n.cf <- length(d.t[d.t <= 3600]) # Number of samples in one hour
  n.an <- length(agg.names)
  for (i.an in 1 : n.an) {
    ans <- agg.names[[i.an]]
    lbl <- agg.labels[i.an]

    ## Filter the aggregated measurement, if requested
    if (do.filter) {
      clams.msr[ans] <- filter(clams.msr[ans], rep(1, n.cf) / n.cf, method="convolution", sides=1)
    }

    ## Aggregate aligned measurement columns
    clams.msr[[lbl]] <- apply(clams.msr[ans], 1, mean)
    clams.sem[[lbl]] <- apply(clams.msr[ans], 1, sd) / sqrt(length(ans))
  }

  ## Turn on the graphic device
  pdf(output.file, width=5.5, height=4.5)

  ## Configure and label the axes
  yrng <- range(clams.msr[agg.labels], na.rm=TRUE)
  yint <- diff(yrng)
  ylim <- yrng + yint * c(-0.120, 0.040)
  xlim <- range(d.t / 3600)
  plot(d.t / 3600, clams.msr[[agg.labels[1]]],
       type="l", col="white", bty="l", tcl=0.5, xpd=TRUE,
       xaxs="r", xlim=xlim, yaxs="r", ylim=ylim,
       xlab="Time, h", ylab=msr.label)
  
  ## Fill rectangles corresponding to periods of light and dark
  col <- list(LIGHT="gray98", DARK="gray92")
  int <- 1 : length(d.t)
  for (ilm in c("LIGHT", "DARK")) {
    idx.ilm <- clams.msr[[ilm]]
    idx.dff <- diff(int[idx.ilm]) > 1
    d.t.min <- min(d.t[idx.ilm])
    d.t.beg <- c(d.t.min, d.t[idx.ilm][c(FALSE, idx.dff)])
    d.t.max <- max(d.t[idx.ilm])
    d.t.end <- c(d.t[idx.ilm][c(idx.dff, FALSE)], d.t.max)
    n.int <- length(d.t.beg)
    for (i.int in seq(1, n.int)) {
      polygon(c(d.t.beg[i.int], d.t.beg[i.int], d.t.end[i.int], d.t.end[i.int]) / 3600,
              c(ylim[1], ylim[2], ylim[2], ylim[1]), col=col[[ilm]], border=NA)
    }
  }

  ## Label the conditions
  yarr <- ylim[2] + yint * 0.175
  ytxt <- ylim[2] + yint * 0.200
  n.cn <- length(con.names)
  for (i.cn in 1 : n.cn) {
    con <- con.names[i.cn]
    lbl <- con.labels[i.cn]
    idx.con <- clams.msr[[con]]
    d.t.con <- range(d.t[idx.con])
    suppressMessages(arrows(d.t.con[1] / 3600, yarr, d.t.con[2] / 3600, yarr,
                            length=0.10, angle=20, code=3, xpd=TRUE))
    text(mean(d.t.con) / 3600, ytxt, pos=3, lbl, xpd=TRUE)
  }

  ## Plot the measurments, and error bars, if requested, and a legend.
  col <- c("blue", "green", "red", "cyan", "magenta", "yellow", "black", "white")
  n.an <- length(agg.names)
  i.do <- 0
  for (i.an in 1 : n.an) {
    lbl <- agg.labels[i.an]
    lines(d.t / 3600, clams.msr[[lbl]], type="l", col=col[i.an])
    if (do.error.bars) {
      ym <- clams.msr[[lbl]] - clams.sem[[lbl]] / 2
      yp <- clams.msr[[lbl]] + clams.sem[[lbl]] / 2
      int <- seq(n.cf + (i.an - 1) * n.cf, length(d.t), 2 * n.cf)
      suppressWarnings(
        arrows(d.t[int] / 3600, ym[int], d.t[int] / 3600, yp[int],
               length=0.05, angle=90, code=3, col=col[i.an]))
    }
  }
  legend("topleft", agg.labels, lty=rep(1, n.an), col=col[1 : n.an], inset=c(0.02, 0.02), xpd=TRUE, bty="n")

  ## Turn off the graphic device
  dev.off()
  status <- TRUE
}