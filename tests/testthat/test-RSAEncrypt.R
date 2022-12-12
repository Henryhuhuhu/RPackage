test_that("RSAEncrypt", {
  expect_equal(paste(RSAEncrypt("Hello World!", 1153, 1517), collapse = ""), "ͶҖĥĥă ȓărĥ®ԋ")
})
