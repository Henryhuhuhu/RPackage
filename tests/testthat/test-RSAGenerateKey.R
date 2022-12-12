test_that("RSAGenerateKey", {
  keylist = RSAPackage::RSAGenerateKey()
  expect_false(is.null(unlist(keylist)[1]))
  expect_false(is.null(unlist(keylist)[2]))
  expect_false(is.null(unlist(keylist)[3]))
})
test_that("equal primes", {
  expect_error(RSAPackage::RSAGenerateKey(7, 7))
})
test_that("negative", {
  expect_error(RSAPackage::RSAGenerateKey(-7, -5))
})

test_that("non numeric", {
  expect_error(RSAPackage::RSAGenerateKey("one", "two"))
})