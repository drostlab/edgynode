# test_that("Test: network_make_binary() -> does transformation from weighted to binary work correctly", {
#   
#   # path to PPCOR output file
#   ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
#   # import PPCOR specific output
#   ppcor_parsed <- ppcor(ppcor_output)
#   # rescale the matrix
#   ppcor_rescaled <- network_rescale(ppcor_parsed)
#   # make the binary matrix
#   ppcor_binary <- network_make_binary(ppcor_rescaled, 70)
#   
#   expect_equal(nrow(ppcor_binary), 19)
#   expect_equal(ncol(ppcor_binary), 19)
#   expect_true(all(apply(ppcor_binary, c(1,2), function (x) x<1.1)))
#   
# })