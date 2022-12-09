#' RSAEncrypt
#'
#' @param plaintext - the vector of characters to be encrypted
#' @param e - the public key encryption component
#' @param n - the key product component
#'
#' @return ciphertext - the encrypted plaintext
#' @export
#'
#' @examples RSAEncrypt(plaintext, 2, 14)
RSAEncrypt <- function(plaintext, e, n){
  #compatibility checks
  
  #calling cpp function
  ciphertext = messageEncrypt(plaintext, e, n)
  return(ciphertext)
}

#' RSADecrypt
#'
#' @param ciphertext - the vector of characters to be decrypted
#' @param d - the private key decryption component
#' @param n - the key product component
#'
#' @return plaintext - the decrypted ciphertext
#' @export
#'
#' @examples
RSADecrypt <- function(ciphertext, d, n){
  #compatibility checks
  
  #calling cpp function
  plaintext = messageDecrypt(ciphertext, d, n)
  return(plaintext)
}

#' RSAGenerateKey
#'
#' @param p - optional first prime number
#' @param q - optional second prime number
#'
#' @return keys = (n, e, d) in which n is the key product component, e is the public key encryption component, and d is the private key decryption component 
#' @export
#'
#' @examples
RSAGenerateKey <- function(p = 2, q = 7){
  #compatibility checks
  
  #calling cpp function
  keys = keyGenerator(p, q)
  return(keys)
}

#' RSAPrimeGenerate
#'
#' @param n - the number of bits for the prime number to have
#'
#' @return - list containing two unique prime numbers of n bit size
#' @export
#'
#' @examples
RSAPrimeGenerate <- function(n = 4){
  #compatibility checks
  
  #calling cpp function
  p = generatePrime(n)
  q = p
  while(p == q){
    q = generatePrime(n)
  }
  return (list(p = p, q = q))
}