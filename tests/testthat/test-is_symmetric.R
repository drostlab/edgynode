test_that("Test: is_symmetric() ...", {
  
  # test that raw input matrix fails adjacency matrix test before being converted into a edgynode adjacency object
  expect_error(edgynode::is_symmetric(edgynode::adjacency_matrix_test_3))
  
  # test that raw input matrix is a non-symmetric adjacency matrix after being converted into a edgynode adjacency object
  expect_false(edgynode::is_symmetric(
    edgynode::make_adjacency(edgynode::adjacency_matrix_test_3)
  ))
  
  # test that raw input matrix is an adjacency matrix after being converted into a edgynode adjacency object
  expect_true(edgynode::is_adjacency(edgynode::make_standard(
    edgynode::make_adjacency(edgynode::adjacency_matrix_test_3)
  )))
  
  # test that raw input matrix is an adjacency matrix after being converted into a edgynode adjacency object and then made standard
  expect_true(edgynode::is_adjacency(edgynode::make_standard(
    edgynode::make_standard(
      edgynode::make_adjacency(edgynode::adjacency_matrix_test_3)
    )
  )))
  
  # test that clean edgynode-standardized input matrix is an adjacency matrix
  expect_true(edgynode::is_adjacency(edgynode::adjacency_clean_test_3))
  
  # test that an already clean edgynode-standardized input matrix when attempted to be cleaned fails
  expect_error(edgynode::is_adjacency(edgynode::make_adjacency(edgynode::adjacency_clean_test_3)))
  
  # test that clean edgynode-standardized input matrix is an adjacency matrix after being made symmetrical
  expect_true(edgynode::is_adjacency(
    edgynode::make_symmetric(edgynode::adjacency_clean_test_3)
  ))
  
  # test that edgynode-standardizes and binarized input matrix is an adjacency matrix
  expect_true(edgynode::is_adjacency(edgynode::make_adjacency(edgynode::adjacency_matrix_genie3_10)))
  
  
})
