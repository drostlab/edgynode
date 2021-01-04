context("Test: network_statistics() ")

test_that("the network_statistics function is working correctly", {

  # path to PIDC output file
  pidc_output <- system.file('beeline_examples/PIDC/outFile.txt', package = 'edgynode')
  # import PIDC output into adjacency matrix
  pidc_parsed <- pidc(pidc_output)
  # rescaling matrix
  pidc_rescaled <- network_rescale(pidc_parsed)
  
  #Tests for network_statistics_...( ) functions
  
  expect_equal(length(network_statistics_component_distribution(pidc_rescaled)), 20)
  expect_equal(length(network_statistics_degree_distribution_naive(pidc_rescaled)), 2)
  expect_equal(length(network_statistics_powerlaws(pidc_rescaled)), 6)

})
