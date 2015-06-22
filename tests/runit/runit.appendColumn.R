clams.data.a <- loadClamsFile(file.path(data.dir, "Collection-2013-01-15/2013-01-15.0101.CSV"))

clams.data.b <- selectColumns(clams.data.a)

.setup <- function () {
}

test.FailsWithInvalidInput <- function () {
  checkException(appendColumn(clams.data.b, c("light", "dark"), TRUE,
                              start.str="06:00:00 AM", stop.str="06:00:00 PM", is.daily=TRUE))
  checkException(appendColumn(clams.data.b, 0, TRUE,
                              start.str="06:00:00 AM", stop.str="06:00:00 PM", is.daily=TRUE))
  checkException(appendColumn(clams.data.b, "light", c(TRUE, TRUE),
                              start.str="06:00:00 AM", stop.str="06:00:00 PM", is.daily=TRUE))
  checkException(appendColumn(clams.data.b, "light", TRUE,
                              start.str=c("06:00:00 AM", "06:00:00 PM"), stop.str="06:00:00 PM", is.daily=TRUE))
  checkException(appendColumn(clams.data.b, "light", TRUE,
                              start.str=0, stop.str="06:00:00 PM", is.daily=TRUE))
  checkException(appendColumn(clams.data.b, "light", TRUE,
                              start.str="06:00:00 AM", stop.str=c("06:00:00 PM", "06:00:00 AM"), is.daily=TRUE))
  checkException(appendColumn(clams.data.b, "light", TRUE,
                              start.str="06:00:00 AM", stop.str=0, is.daily=TRUE))
  checkException(appendColumn(clams.data.b, "light", TRUE,
                              start.str="06:00:00 AM", stop.str="06:00:00 PM", is.daily=c(TRUE, FALSE)))
  checkException(appendColumn(clams.data.b, "light", TRUE,
                              start.str="06:00:00 AM", stop.str="06:00:00 PM", is.daily=0))
}

test.AcceptsOnlyValidStartStrs <- function () {
  clams.data.c <- appendColumn(clams.data.b, "light", TRUE);

  ## Prepends a date.time object D.T
  checkTrue(is.null(clams.data.b$measurements$D.T))
  checkTrue(!is.null(clams.data.c$measurements$D.T))

  ## Uses the measurement start time
  checkTrue(clams.data.c$measurements$LIGHT[1])
  
  ## Uses the measurement start date with start time of day
  start.str <- format(strptime(clams.data.b$measurements$DATE.TIME[3], "%m/%d/%Y %I:%M:%S %p"), "%I:%M:%S %p")
  clams.data.c <- appendColumn(clams.data.b, "light", TRUE, start.str=start.str)
  checkTrue(!clams.data.c$measurements$LIGHT[1])
  checkTrue(!clams.data.c$measurements$LIGHT[2])
  checkTrue(clams.data.c$measurements$LIGHT[3])
  checkTrue(clams.data.c$measurements$LIGHT[4])
  checkTrue(clams.data.c$measurements$LIGHT[5])

  ## Uses the date and time of day
  start.str <- format(strptime(clams.data.b$measurements$DATE.TIME[30], "%m/%d/%Y %I:%M:%S %p"), "%m/%d/%Y %I:%M:%S %p")
  clams.data.c <- appendColumn(clams.data.b, "light", TRUE, start.str=start.str)
  checkTrue(!clams.data.c$measurements$LIGHT[28])
  checkTrue(!clams.data.c$measurements$LIGHT[29])
  checkTrue(clams.data.c$measurements$LIGHT[30])
  checkTrue(clams.data.c$measurements$LIGHT[31])
  checkTrue(clams.data.c$measurements$LIGHT[32])

  ## Accepts correct start str format only
  ## appendColumn(clams.data.b, "light", TRUE, start.str="1/15/2013 7:11:49 PM")
  checkException(appendColumn(clams.data.b, "light", TRUE, start.str="1/15/2013 7:11:49"))
  checkException(appendColumn(clams.data.b, "light", TRUE, start.str="1/15/2013 7:11 PM"))
  checkException(appendColumn(clams.data.b, "light", TRUE, start.str="1/15/2013 7:11:49 P"))
}

test.AcceptsOnlyValidStopStrs <- function () {
  clams.data.c <- appendColumn(clams.data.b, "light", TRUE);
  n.data <- length(clams.data.c$measurements$D.T)

  ## Uses the measurement stop time
  checkTrue(clams.data.c$measurements$LIGHT[n.data - 1])
  checkTrue(!clams.data.c$measurements$LIGHT[n.data])

  ## Uses the measurement start date with stop time of day
  stop.str <- format(strptime(clams.data.b$measurements$DATE.TIME[6], "%m/%d/%Y %I:%M:%S %p"), "%I:%M:%S %p")
  clams.data.c <- appendColumn(clams.data.b, "light", TRUE, stop.str=stop.str)
  checkTrue(clams.data.c$measurements$LIGHT[4])
  checkTrue(clams.data.c$measurements$LIGHT[5])
  checkTrue(!clams.data.c$measurements$LIGHT[6])
  checkTrue(!clams.data.c$measurements$LIGHT[7])
  checkTrue(!clams.data.c$measurements$LIGHT[8])

  ## Uses the date and time of day
  stop.str <- format(strptime(clams.data.b$measurements$DATE.TIME[60], "%m/%d/%Y %I:%M:%S %p"), "%m/%d/%Y %I:%M:%S %p")
  clams.data.c <- appendColumn(clams.data.b, "light", TRUE, stop.str=stop.str)
  checkTrue(clams.data.c$measurements$LIGHT[58])
  checkTrue(clams.data.c$measurements$LIGHT[59])
  checkTrue(!clams.data.c$measurements$LIGHT[60])
  checkTrue(!clams.data.c$measurements$LIGHT[61])
  checkTrue(!clams.data.c$measurements$LIGHT[62])

  ## Adds a day's worth of seconds
  ## > format(strptime(clams.data.b$measurements$DATE.TIME[6], "%m/%d/%Y %I:%M:%S %p"), "%m/%d/%Y %I:%M:%S %p")
  ## [1] "01/15/2013 06:29:49 PM"
  start.str <- "06:29:49 PM"
  ## > format(strptime(clams.data.b$measurements$DATE.TIME[3], "%m/%d/%Y %I:%M:%S %p"), "%m/%d/%Y %I:%M:%S %p")
  ## [1] "01/15/2013 05:58:19 PM"
  stop.str <- "05:58:19 PM"
  clams.data.c <- appendColumn(clams.data.b, "light", TRUE, start.str=start.str, stop.str=stop.str)
  stop.str <- "01/16/2013 05:58:19 PM"
  clams.data.d <- appendColumn(clams.data.b, "light", TRUE, start.str=start.str, stop.str=stop.str)
  checkEquals(clams.data.c$measurements$LIGHT, clams.data.d$measurements$LIGHT)
  
  ## Accepts correct stop str format only
  checkException(appendColumn(clams.data.b, "light", TRUE, stop.str="1/15/2013 7:11:49"))
  checkException(appendColumn(clams.data.b, "light", TRUE, stop.str="1/15/2013 7:11 PM"))
  checkException(appendColumn(clams.data.b, "light", TRUE, stop.str="1/15/2013 7:11:49 P"))

  ## Ensures the start time is earlier than the stop time
  start.str <- "01/15/2013 06:29:49 PM"
  stop.str <- "01/15/2013 05:58:19 PM"
  checkException(appendColumn(clams.data.b, "light", TRUE, start.str=start.str, stop.str=stop.str))

  ## If the time interval recurs daily, ensure the start and stop times are time of day
  start.str <- "01/15/2013 05:58:19 PM"
  stop.str <- "01/15/2013 06:29:49 PM"
  checkException(appendColumn(clams.data.b, "light", TRUE, start.str=start.str, stop.str=stop.str, is.daily=TRUE))
  start.str <- "05:58:19 PM"
  stop.str <- "01/15/2013 06:29:49 PM"
  checkException(appendColumn(clams.data.b, "light", TRUE, start.str=start.str, stop.str=stop.str, is.daily=TRUE))
  start.str <- "01/15/2013 05:58:19 PM"
  stop.str <- "06:29:49 PM"
  checkException(appendColumn(clams.data.b, "light", TRUE, start.str=start.str, stop.str=stop.str, is.daily=TRUE))
}

test.AppendsColumn <- function () {
  clams.data.c <- appendColumn(clams.data.b, "boolean", TRUE)
  clams.data.d <- appendColumn(clams.data.c, "double", 1.0)
  clams.data.e <- appendColumn(clams.data.d, "character", "a")
  n.data = length(clams.data.b$measurements$DATE.TIME)
  checkEquals(clams.data.c$measurements$BOOLEAN, c(rep(TRUE, n.data - 1), FALSE))
  checkEquals(clams.data.d$measurements$DOUBLE, c(rep(1.0, n.data - 1), 0.0))
  checkEquals(clams.data.e$measurements$CHARACTER, c(rep("a", n.data - 1), ""))
}

test.ReturnsClamsDataOrCollection <- function () {
  clams.coll.a <- loadClamsDir(file.path(data.dir, "Collection-2013-01-15"))
  clams.coll.b <- selectColumns(clams.coll.a)
  clams.coll.c <- appendColumn(clams.coll.b, "light", TRUE, start.str="06:00:00 AM", stop.str="06:00:00 PM", is.daily=TRUE)
  checkEquals(names(clams.coll.c), c("2013-01-15.0101.CSV",
                                     "2013-01-15.0102.CSV",
                                     "2013-01-15.0103.CSV",
                                     "2013-01-15.0104.CSV",
                                     "2013-01-15.0105.CSV",
                                     "2013-01-15.0106.CSV"))
                                   
  clams.data.c <- appendColumn(clams.data.b, "light", TRUE, start.str="06:00:00 AM", stop.str="06:00:00 PM", is.daily=TRUE)
  checkEquals(names(clams.data.c), c("meta.data",
                                     "measurements"))
}

.teardown <- function () {
}
