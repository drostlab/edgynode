context("Test: network_statistics() ")

test_that("the network_statistics function is working correctly", {

  # path to PPCOR output file
  ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'scNetworkR')
  # import PPCOR output into adjacency matrix
  ppcor_parsed <- ppcor(ppcor_output)
  # calculate network statistics
  ppcor_statistics <- network_statistics(ppcor_parsed)
  # visualize
  ppcor_statistics

  expect_equal(length(ppcor_statistics), 5)

})
