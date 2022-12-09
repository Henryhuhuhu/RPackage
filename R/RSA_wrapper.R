RSAEncrypt <- function(plaintext, e, n){
  #compatibility checks
  
  #calling cpp function
  ciphertext = messageEncrypt(plaintext, e, n)
  return(ciphertext)
}

RSADecrypt <- function(ciphertext, d, n){
  #compatibility checks
  
  #calling cpp function
  plaintext = messageDecrypt(ciphertext, d, n)
  return(plaintext)
}

RSAGenerateKey <- function(p = 2, q = 7){
  #compatibility checks
  
  #calling cpp function
  keys = keyGenerator(p, q)
  return(keys)
}

RSAPrimeGenerate <- function(n = 4){
  #compatibility checks
  
  #calling cpp function
  p = generatePrime(n)
  q = p
  while(p == q){
    q = generatePrime(n)
  }
}