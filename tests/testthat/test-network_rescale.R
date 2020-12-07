context("Test: network_rescale() ")

test_that("the rescale adjacency matrix function is working correctly", {

  # path to PIDC output file
  pidc_output <- system.file('beeline_examples/PIDC/outFile.txt', package = 'edgynode')
  # parsing the output to an adjacency matrix
  pidc_parsed <- pidc(pidc_output)
  # rescaling the matrix
  rescaled <- network_rescale(pidc_parsed)
  # result
  expect_output(print(rescaled))
  # symmetry
  expect_true(isSymmetric(rescaled))

  expect_equal(nrow(rescaled), 19)
  expect_equal(ncol(rescaled), 19)
  expect_true(all(apply(rescaled, 2, is.numeric)))

})
