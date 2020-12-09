context("Test: network_make_binary() ")

test_that("transformation from weighted to binary is working correctly", {
  
  # path to PPCOR output file
  ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
  # import PPCOR specific output
  ppcor_parsed <- ppcor(ppcor_output)
  # rescale the matrix
  ppcor_rescaled <- network_rescale(ppcor_parsed)
  # make the binary matrix
  ppcor_binary <- network_make_binary(ppcor_rescaled, 70)
  
  expect_equal(nrow(ppcor_binary), 19)
  expect_equal(ncol(ppcor_binary), 19)
  expect_that(ppcor_binary, 1, all(apply(is_less_than(1.1))))
  
})