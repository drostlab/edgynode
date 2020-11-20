context("Test: ppcor() ")

test_that("ppcor() correctly imports BEELINE PPCOR output.", {

  ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'scNetworkR')
  # import PPCOR specific output
  ppcor_parsed <- ppcor(ppcor_output)

  expect_equal(nrow(ppcor_parsed), 19)
  expect_equal(ncol(ppcor_parsed), 19)
  expect_true(all(is.character(row.names(ppcor_parsed))))
  expect_true(all(apply(ppcor_parsed, 2, is.numeric)))
  expect_error(ppcor("path/that/does/not/exist"))
  expect_true(isSymmetric(ppcor_parsed) == TRUE)

})
