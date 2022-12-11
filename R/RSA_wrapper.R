#' RSAEncrypt
#'
#' @param plaintext - the string of characters to be encrypted
#' @param e - the public key encryption component
#' @param n - the key product component
#'
#' @return ciphertext - the encrypted plaintext as a character vector
#' @export
#'
#' @examples RSAEncrypt("Hello World!", 1153, 1517)
RSAEncrypt <- function(plaintext, e, n){
  #compatibility checks
  if(is.numeric(n) == FALSE || is.numeric(e) == FALSE){
    stop("Inputs needs to be a number")
  }
  if((n < 1) || (e < 1)){
    stop("Inputs needs to be positive")
  }
  #calling cpp function
  ciphertext = messageEncrypt(utf8ToInt(plaintext), e, n)
  #turn the returned ascii values to ascii characters
  return(intToUtf8(ciphertext, multiple = TRUE))
}

#' RSADecrypt
#'
#' @param ciphertext - the string of characters to be decrypted
#' @param d - the private key decryption component
#' @param n - the key product component
#'
#' @return plaintext - the decrypted ciphertext as a character vector
#' @export
#'
#' @examples RSADecrypt("ͶҖĥĥă ȓărĥ®ԋ", 577, 1517)
RSADecrypt <- function(ciphertext, d, n){
  #compatibility checks
  if(is.numeric(n) == FALSE || is.numeric(d) == FALSE){
    stop("Inputs needs to be a number")
  }
  if((n < 1) || (d < 1)){
    stop("Inputs needs to be positive")
  }
  #calling cpp function
  plaintext = messageDecrypt(utf8ToInt(paste(ciphertext,collapse = "")), d, n)
  #turn the returned ascii values to ascii characters
  return(intToUtf8(plaintext, multiple = TRUE))
}

#' RSAGenerateKey
#'
#' @param p - first prime number
#' @param q - second prime number
#'
#' @return keys = (n, e, d) in which n is the key product component, e is the public key encryption component, and d is the private key decryption component 
#' @export
#'
#' @examples RSAGenerateKey(41, 37)
RSAGenerateKey <- function(p = NULL, q = NULL){
  #compatibility checks
  if(is.null(p) && is.null(q)){
    primeList = RSAPrimeGenerate(6)
    p = unlist(primeList)[1]
    q = unlist(primeList)[2]
  }else{ 
    if(p == q){
      stop("Inputs need to be unique")
    }
    if(is.numeric(p) == FALSE || is.numeric(q) == FALSE){
      stop("Inputs need to be a number")
    }
    if((p < 2) || (q < 2)){
      stop("Inputs need to be prime")
    }
    if(rPrimeCheck(as.integer(p)) == FALSE || rPrimeCheck(as.integer(q)) == FALSE){
      #stop("Inputs need to be prime")
    }
  }
  #calling cpp function
  keys = keyGenerator(as.integer(p), as.integer(q))
  return(keys)
}

#' RSAPrimeGenerate
#'
#' @param n - the number of bits for the prime number to have
#'
#' @return list containing two unique prime numbers of n bit size
#' @export
#'
#' @examples  RSAPrimeGenerate(6)
RSAPrimeGenerate <- function(n = 6){
  #compatibility checks
  if(is.numeric(n) == FALSE){
    stop("Input needs to be a number")
  }
  if(n < 1){
    stop("Input needs to be positive")
  }
  #converting to integer value
  n = as.integer(n)
  #calling cpp function
  p = generatePrime(n)
  repeat{
    #making sure that p and q are unique
    q = generatePrime(n)
    if (p != q) break;
  }
  return (list(p, q))
}