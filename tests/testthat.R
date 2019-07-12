library(testthat)
library(RASminer)

test.quasi = test_path("sample-data", "SampleQuasiUnsteady.hdf")
test.unsteady = test_path("sample-data", "SampleUnsteady.hdf")

test_check("RASminer")
