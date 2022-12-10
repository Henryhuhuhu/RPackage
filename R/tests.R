Generaltest <- function(){
  plaintext = "Hello World!"
  primeList = RSAPrimeGenerate(6)
  p = unlist(primeList)[1]
  q = unlist(primeList)[2]
  print(primeList)
  keyList = RSAGenerateKey(p, q)
  n = unlist(keyList)[1]
  e = unlist(keyList)[2]
  d = unlist(keyList)[3]
  print(keyList)
  ciphertext = RSAEncrypt(plaintext, e, n)
  print(ciphertext)
  decoded = RSADecrypt(ciphertext, d, n)
  print(decoded)
}