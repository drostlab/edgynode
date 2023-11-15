# test_that("Test: network_compare() function is working correctly", {
# 
#   # path to PPCOR output file
#   ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
#   # path to PIDC output file
#   pidc_output <- system.file('beeline_examples/PIDC/outFile.txt', package = 'edgynode')
#   # import PPCOR specific output
#   ppcor_parsed <- ppcor(ppcor_output)
#   # import PIDC specific output
#   pidc_parsed <- pidc(pidc_output)
#   # rescaling PPCOR output
#   ppcor_rescaled <- network_rescale(ppcor_parsed)
#   # rescaling PIDC output
#   pidc_rescaled <- network_rescale(pidc_parsed)
#   # making the comparisons
#   
#   expect_true(is.numeric(network_compare(ppcor_rescaled, pidc_rescaled)))
#   expect_false(network_compare(ppcor_rescaled, pidc_rescaled) == 0)
#   expect_true(is.numeric(network_compare(ppcor_rescaled, pidc_rescaled, comparison_method = "nmi")))
#   expect_false(network_compare(ppcor_rescaled, pidc_rescaled, comparison_method = "nmi") == 0)
#   expect_true(is.numeric(network_compare(ppcor_rescaled, pidc_rescaled, comparison_method = "split.join")))
#   expect_false(network_compare(ppcor_rescaled, pidc_rescaled, comparison_method = "split.join") == 0)
#   expect_true(is.numeric(network_compare(ppcor_rescaled, pidc_rescaled, comparison_method = "rand")))
#   expect_false(network_compare(ppcor_rescaled, pidc_rescaled, comparison_method = "rand") == 0)
#   expect_true(is.numeric(network_compare(ppcor_rescaled, pidc_rescaled, comparison_method = "adjusted.rand")))
#   expect_false(network_compare(ppcor_rescaled, pidc_rescaled, comparison_method = "adjusted.rand") == 0)
# })
