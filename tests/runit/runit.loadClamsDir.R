.setup <- function () {
}

test.FailsWhenDirDoesNotExist <- function () {
  checkException(loadClamsDir(file.path(data.dir, "Dir.Does.Not.Exist")))
}

test.LoadsEmptyDir <- function () {
  clams.coll <- loadClamsDir(file.path(data.dir, "Empty.Dir"))
  checkEquals(clams.coll, list())
}

test.LoadsDir <- function () {
  clams.coll <- loadClamsDir(file.path(data.dir, "Collection-2013-01-15"))

  checkEquals(names(clams.coll), c("2013-01-15.0101.CSV",
                                   "2013-01-15.0102.CSV",
                                   "2013-01-15.0103.CSV",
                                   "2013-01-15.0104.CSV",
                                   "2013-01-15.0105.CSV",
                                   "2013-01-15.0106.CSV"))
                                   
  clams.data <- loadClamsFile(file.path(data.dir, "Collection-2013-01-15/2013-01-15.0101.CSV"))
  checkEquals(clams.coll[["2013-01-15.0101.CSV"]], clams.data)
  
  clams.data <- loadClamsFile(file.path(data.dir, "Collection-2013-01-15/2013-01-15.0106.CSV"))
  checkEquals(clams.coll[["2013-01-15.0106.CSV"]], clams.data)
}

.teardown <- function () {
}
