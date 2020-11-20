context("Test: pidc() ")

test_that("pidc() correctly imports BEELINE PIDC output.", {

  pidc_output <- system.file('beeline_examples/PIDC/outFile.txt', package = 'scNetworkR')
  # import PIDC specific output
  pidc_parsed <- pidc(pidc_output)

  expect_equal(nrow(pidc_parsed), 19)
  expect_equal(ncol(pidc_parsed), 19)
  expect_true(all(is.character(row.names(pidc_parsed))))
  expect_true(all(apply(pidc_parsed, 2, is.numeric)))
  expect_error(pidc("path/that/does/not/exist"))
  expect_true(isSymmetric(pidc_parsed) == TRUE)

})
