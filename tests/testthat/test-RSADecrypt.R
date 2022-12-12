test_that("RSADecrypt", {
  expect_equal(paste(RSADecrypt("ͶҖĥĥă ȓărĥ®ԋ", 577, 1517), collapse = ""), "Hello World!")
})
test_that("Non numeric", {
  expect_error(RSADecrypt("ͶҖĥĥă ȓărĥ®ԋ", "five", "four"))
})
test_that("negative", {
  expect_error(RSADecrypt("ͶҖĥĥă ȓărĥ®ԋ", -3, -5))
})