context("Test: genie() ")

test_that("genie() correctly imports BEELINE GENIE3 output.", {

 genie3_output <- system.file('beeline_examples/GENIE3/outFile.csv', package = 'edgynode')
  # import GENIE3 specific output
 genie3_parsed <- genie(genie3_output)

 expect_equal(nrow(genie3_parsed), 1000)
 expect_equal(ncol(genie3_parsed), 1000)
 expect_true(all(is.character(row.names(genie3_parsed))))
 expect_true(all(apply(genie3_parsed, 2, is.numeric)))
 expect_error(genie("path/that/does/not/exist"))
 expect_true(isSymmetric(network_rescale(genie3_parsed)))

})
