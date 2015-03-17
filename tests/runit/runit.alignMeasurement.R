.setup <- function () {
}

test.FailsWithInvalidInput <- function () {
  clams.coll.a <- loadClamsDir(file.path(data.dir, "Collection-2013-01-15"))
  clams.coll.b <- selectColumns(clams.coll.a)
  clams.coll.c <- appendColumn(clams.coll.b, "light", TRUE, start.str="06:00:00 AM", stop.str="06:00:00 PM", is.daily=TRUE)
  checkException(alignMeasurement(clams.coll.c, c("VO2", "VCO2")))
  checkException(alignMeasurement(clams.coll.c, 0.0))
  checkException(alignMeasurement(clams.coll.c, "VO2", lbl.names=0.0))
  checkException(alignMeasurement(clams.coll.c, "VO2", sel.names="VO2"))
  checkException(alignMeasurement(clams.coll.c, "VO2", sel.names=c("VO2", "VCO2"), sel.condition=c(0.0, 1.0)))
  checkException(alignMeasurement(clams.coll.c, "VO2", sel.names=0.0, sel.condition=c(0.0, 1.0)))
  checkException(alignMeasurement(clams.coll.c, "VO2", sel.condition=c(0.0, 1.0)))
  checkException(alignMeasurement(clams.coll.c, "VO2", sel.names="VO2", sel.condition=c(0.0, 1.0, 2.0)))
}

test.AlignsMeasurments <- function () {
  clams.data.a <- loadClamsFile(file.path(data.dir, "Alignment/2013-01-15.0101.CSV"))
  clams.data.b <- loadClamsFile(file.path(data.dir, "Alignment/2013-01-15.0101_Decimated.CSV"))

  clams.coll.a <- list()
  clams.coll.a[["a"]] <- clams.data.a
  clams.coll.a[["b"]] <- clams.data.b
  clams.coll.b <- selectColumns(clams.coll.a)
  clams.coll.c <- appendColumn(clams.coll.b, "light", TRUE, start.str="06:00:00 AM", stop.str="06:00:00 PM", is.daily=TRUE)

  clams.msr.a <- alignMeasurement(clams.coll.c, "VO2", lbl.names="LIGHT")

  ## Assign seconds, measurements, and labels columns, with subject identifier for measurement column names
  checkEquals(names(clams.msr.a),
              c("D.T", gsub(" ", ".", c(clams.data.a$meta.data$subject.id,
                                        clams.data.b$meta.data$subject.id)), "LIGHT"))

  ## Assign a measurement time sequence in seconds from the start of measurement
  checkEquals(clams.msr.a$D.T,
              as.double(difftime(clams.coll.c[["a"]]$measurements$D.T,
                                 clams.coll.c[["a"]]$measurements$D.T[1], units="sec")));


  ## Use the most frequent sampling
  checkEquals(clams.msr.a[[gsub(" ", ".", clams.data.a$meta.data$subject.id)]],
              clams.coll.c[["a"]]$measurements$VO2);


  ## Every other value agrees by construction
  n.data <- length(clams.msr.a[[gsub(" ", ".", clams.data.a$meta.data$subject.id)]])
  checkEquals(clams.msr.a[[gsub(" ", ".", clams.data.a$meta.data$subject.id)]][seq(1, n.data, 2)],
              clams.msr.a[[gsub(" ", ".", clams.data.b$meta.data$subject.id)]][seq(1, n.data, 2)])
  checkTrue(all(clams.msr.a[[gsub(" ", ".", clams.data.b$meta.data$subject.id)]][seq(2, n.data, 2)] == 0))
}

.teardown <- function () {
}
