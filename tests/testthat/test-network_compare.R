context("Test: network_compare() ")

test_that("the network_compare() function is working correctly", {

  # path to PPCOR output file
  ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'scNetworkR')
  # path to PIDC output file
  pidc_output <- system.file('beeline_examples/PIDC/outFile.txt', package = 'scNetworkR')
  # import PPCOR specific output
  ppcor_parsed <- ppcor(ppcor_output)
  # import PIDC specific output
  pidc_parsed <- pidc(pidc_output)
  # rescaling PPCOR output
  ppcor_rescaled <- network_rescale(ppcor_parsed)
  # rescaling PIDC output
  pidc_rescaled <- network_rescale(pidc_parsed)
  # making the comparison
  compared <- network_compare(ppcor_rescaled, pidc_rescaled)

  expect_equal(length(compared), 5)
  expect_true(all(is.double(compared)))

})
