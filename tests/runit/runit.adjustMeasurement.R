clams.coll.a <- loadClamsDir(file.path(data.dir, "Collection-2015-01-06"))

clams.coll.a <- selectColumns(clams.coll.a, do.remove.outliers=TRUE) 

orig.weight <- c(23.8, 25.8, 22.6, 25.5, 25.5)
adj.weight <- c(23.5, 24.3, 22.6, 22.4, 24.9)

clams.coll.a <- appendColumn(clams.coll.a, "orig.weight", orig.weight)
clams.coll.a <- appendColumn(clams.coll.a, "adj.weight", adj.weight,
                             start.str="01/08/2015 06:00:00 AM", stop.str="01/09/2015 06:00:00 AM")

clams.data.a <- clams.coll.a[[1]]

.setup <- function () {
}

test.FailsWithInvalidInput <- function () {
  checkException(adjustMeasurement(0.0, "VO2"))
  checkException(adjustMeasurement(clams.coll.a, c("VO2", "VCO2")))
  checkException(adjustMeasurement(clams.coll.a, 0.0))
  checkException(adjustMeasurement(clams.coll.a, "HEAT"))
}

test.adjustMeasurement <- function () {
  clams.coll.b <- adjustMeasurement(clams.coll.a, "VO2")
  n.list <- length(clams.coll.b)
  for (i.list in seq(n.list)) {
    ratio <- clams.coll.b[[i.list]]$measurements["ADJ.VO2"] / clams.coll.b[[i.list]]$measurements["VO2"]
    ratio <- ratio[[1]]
    checkTrue(mean(ratio[ratio != 0 & !is.na(ratio)]) == adj.weight[i.list] / orig.weight[i.list])
  }
}

test.ReturnsClamsDataOrCollection <- function () {
  clams.coll.b <- adjustMeasurement(clams.coll.a, "VO2")
  checkEquals(names(clams.coll.b), c("2015-01-06.0101.CSV",
                                     "2015-01-06.0102.CSV",
                                     "2015-01-06.0103.CSV",
                                     "2015-01-06.0104.CSV",
                                     "2015-01-06.0105.CSV"))
  
  clams.data.b <- adjustMeasurement(clams.data.a, "VO2")
  checkEquals(names(clams.data.b), c("meta.data",
                                     "measurements"))
}

.teardown <- function () {
}
