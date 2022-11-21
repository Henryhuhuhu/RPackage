
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}

int NumberGenerator(int n){
  // random number of size n bits
  int max = pow(2, n) - 1;
  int min = pow(2, n - 1) + 1;
  int number = min + (rand() % ( max - min + 1 ) );
  return number;
}

bool LowLevelPrimeCheck(int number){
  //first 100 prime numbers
  int firstPrimes[100] = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
                  31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
                  73, 79, 83, 89, 97, 101, 103, 107, 109, 113,
                  127, 131, 137, 139, 149, 151, 157, 163, 167, 173,
                  179, 181, 191, 193, 197, 199, 211, 223, 227, 229,
                  233, 239, 241, 251, 257, 263, 269, 271, 277, 281,
                  283, 293, 307, 311, 313, 317, 331, 337, 347, 349,
                  353, 359, 367, 373, 379, 383, 389, 397, 401, 409,
                  419, 421, 431, 433, 439, 443, 449, 457, 461, 463,
                  467, 479, 487, 491, 499, 503, 509, 521, 523, 541};
  for (int divisor : firstPrimes){
    if (number % divisor == 0 && (divisor^2) <= number){
      break;
    }else{
      return 1;
    }
  }
  return 0;
}

bool PrimeCheck(int number){
  if(LowLevelPrimeCheck(number) == 0){
    return 0;
  }
  //Rabin Miller Primality Test
  int d = number - 1;
  while(d % 2 ==0){
    d /= 2;
  }
  int iter = 20;    //Number of iterations of test. Higher value is more accurate
  for(int i = 0; i < iter; i++){
    // Pick a random number in [2 to (number-2)]
    int a = 2 + rand() % (number - 4);
    int modExp = (a ^ d) % number;
    if(modExp == 1 || modExp == number - 1){
      return 1;
    }
    while(d != number - 1){
      modExp = (modExp ^ 2) % number;
      d *= 2;
      if(modExp == 1){
        return 0;
      }
      if(modExp == number - 1){
        return 1;
      }
    }
  }    
  return 0;
}

int PrimeNumberGenerate(int n){
  int number;
  while(1){
    number = NumberGenerator(n);
    if(PrimeCheck(number)){
      return number;
    }
  }
}