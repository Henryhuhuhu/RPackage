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
  ciphertext = messageEncrypt(utf8ToInt(plaintext), e, n)
  #turn the returned ascii values to ascii characters
  return(intToUtf8(ciphertext, multiple = TRUE))
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
  plaintext = messageDecrypt(utf8ToInt(paste(ciphertext,collapse = "")), d, n)
  #turn the returned ascii values to ascii characters
  return(intToUtf8(plaintext, multiple = TRUE))
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
  keys = keyGenerator(as.integer(p), as.integer(q))
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
  repeat{
    q = generatePrime(n)
    if (p != q) break;
  }
  return (list(p, q))
}