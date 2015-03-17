\name{CLAMS-package}
\alias{CLAMS-package}
\alias{CLAMS}
\docType{package}
\title{
  Performs CLAMS Data Analysis
}
\description{
  Provides functions for analyzing data produced by the Columbus
  Instruments Comprehensive Lab Animal Monitoring System (CLAMS).
}
\details{
  \tabular{ll}{
    Package: \tab CLAMS\cr
    Type: \tab Package\cr
    Version: \tab 2.2-3\cr
    Date: \tab 2015-03-14\cr
    License: \tab MIT + file LICENSE\cr
  }
  % TODO: Complete
  % ~~ An overview of how to use the package, including the most important functions~~
}
\author{
  Author: Katherine and Raymond LeClair\cr
  Maintainer: Raymond LeClair <raymond.leclair@gmail.com>
}
% \references{
% }
\keyword{package}
% \seealso{
% }
\examples{
% TODO: Complete
% ~~ simple examples of the most important functions ~~
clams.file <- system.file("extdata", "Test", "Collection", "2013-01-15.0101.CSV", package = "CLAMS")
clams.data <- loadClamsFile(clams.file)
}
