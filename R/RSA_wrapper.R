#' Title
#'
#' @param plaintext 
#' @param e 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
RSAEncrypt <- function(plaintext, e, n){
  #compatibility checks
  
  #calling cpp function
  ciphertext = messageEncrypt(plaintext, e, n)
  return(ciphertext)
}

#' Title
#'
#' @param ciphertext 
#' @param d 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
RSADecrypt <- function(ciphertext, d, n){
  #compatibility checks
  
  #calling cpp function
  plaintext = messageDecrypt(ciphertext, d, n)
  return(plaintext)
}

#' Title
#'
#' @param p 
#' @param q 
#'
#' @return
#' @export
#'
#' @examples
RSAGenerateKey <- function(p = 2, q = 7){
  #compatibility checks
  
  #calling cpp function
  keys = keyGenerator(p, q)
  return(keys)
}

#' Title
#'
#' @param n 
#'
#' @return
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
}