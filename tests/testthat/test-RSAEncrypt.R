test_that("RSAEncrypt", {
  expect_equal(paste(RSAEncrypt("Hello World!", 1153, 1517), collapse = ""), "ͶҖĥĥă ȓărĥ®ԋ")
})
test_that("Non numeric", {
  expect_error(RSAEncrypt("Hello World!", "one", "two"))
})
test_that("negative", {
  expect_error(RSAEncrypt("Hello World!", -1, -2))
})