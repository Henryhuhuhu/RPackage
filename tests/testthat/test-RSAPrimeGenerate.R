test_that("RSAPrimeGenerate", {
  primeList = RSAPrimeGenerate(6)
  p = unlist(primeList)[1]
  q = unlist(primeList)[2]
  for(d in 2:as.integer(p/2)){
    expect_false(p %% d == 0)
  }
  for(d in 2:as.integer(q/2)){
    expect_false(q %% d == 0)
  }
})
test_that("Negative n", {
    expect_error(RSAPrimeGenerate(-1))
})

test_that("Non numeric", {
  expect_error(RSAPrimeGenerate("seven"))
})