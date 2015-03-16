testClams <- function() {
  library("RUnit")
  
  data.dir <- system.file("extdata", "Test", package = "CLAMS")
  
  testSuite <- defineTestSuite(
    "CLAMS", dirs = file.path("tests/runit"), testFileRegexp = "^runit.+\\.[rR]$", testFuncRegexp = "^test.+$",
    rngKind = "Marsaglia-Multicarry", rngNormalKind = "Kinderman-Ramage")
  
  testResult <- runTestSuite(testSuite)

  if (testResult$CLAMS$nErr == 0 && testResult$CLAMS$nFail == 0) {
    TRUE
  } else {
    FALSE
  }
}
testClams()
