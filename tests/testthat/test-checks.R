test_that("check_x", {
  expect_error(check_x(c(-1, 0, 2)))
  expect_equal(check_x(c(0, 2)), c(0L, 2L))
})

test_that("check_w", {
  expect_error(check_w(-0.01))
  expect_error(check_w(1.01))
  expect_no_error(check_w(1e-3))
})

test_that("check_p", {
  expect_error(check_p(-0.01))
  expect_error(check_p(1.01))
  expect_error(check_p(list(c(0.2, 0.7, 0.2))))
  expect_no_error(check_p(list(c(0.2, 0.7, 0.1))))
})


test_that("check_tab", {
  tab <- matrix(c(1000, 10, 2, 12, 100, 8, 1, 7, 200), nrow = 3)
  expect_no_error(check_tab(tab))
  
  tab <- matrix(c(-1, 10, 2, 12, 100, 8, 1, 7, 200), nrow = 3)
  expect_error(check_tab(tab))
  
  tab <- matrix(c(28, 1, 7, 200), nrow = 2)
  expect_error(check_tab(tab))
})
