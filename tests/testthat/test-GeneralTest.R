test_that("GeneralTest", {
  set.seed(1234)
  plaintext = "Hello World!"
  primeList = RSAPrimeGenerate(6)
  p = unlist(primeList)[1]
  q = unlist(primeList)[2]
  keyList = RSAGenerateKey(p, q)
  n = unlist(keyList)[1]
  e = unlist(keyList)[2]
  d = unlist(keyList)[3]
  ciphertext = RSAEncrypt(plaintext, e, n)
  decoded = RSADecrypt(ciphertext, d, n)
  expect_equal(paste(decoded, collapse = ""), plaintext)
})
