
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;



int NumberGenerator(int n){
  // random number of size n bits
  int max = pow(2, n) - 1;
  int min = pow(2, n - 1) + 1;
  int number = min + (rand() % ( max - min + 1 ) );
  return number;
}

int gcd(int a,int b) {
  //Euclidean Algorithm
  int r;
  while ((a % b) > 0)  {
    r = a % b;
    a = b;
    b = r;
  }
  return b;
}

int pow(int a,int b) {
  int powered = 1;
  for(int i = 0; i < b; i++){
    powered *= a;
  }
  return powered;
}

int modExp(int A, int B, int C)
{
  // Base cases
  if (A == 0)
    return 0;
  if (B == 0)
    return 1;
  
  // If B is even
  long long y;
  if (B % 2 == 0) {
    y = modExp(A, B / 2, C);
    y = (y * y) % C;
  }
  
  // If B is odd
  else {
    y = A % C;
    y = (y * modExp(A, B - 1, C) % C) % C;
  }
  
  return (int)((y + C) % C);
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
  //Checking if divisible by first 100 primes 
  for (int divisor : firstPrimes){
    if (number % divisor == 0 && (divisor^2) <= number){
      break;
    }else{
      return 1;
    }
  }
  return 0;
}

bool compositeCheck(int a, int d, int number, int k)
{
  if ((long long)pow(a, d) % number == 1 )
    return 0;
  for (int i = 0; i < k; i++)
  {
    if ((long long)pow(a, (1 << i) * d) % number == number - 1)
      return 0;
  }
  return 1;
}
// [[Rcpp::export]]
bool PrimeCheck(int number){
  if(LowLevelPrimeCheck(number) == 0){
    return 0;
  }
  //Rabin Miller Primality Test
  int d = number - 1;
  int k = 0;
  while(d % 2 ==0){
    d /= 2;
    k++;
  }
  int iter = 5;    //Number of iterations of test. Higher value is more accurate but slower
  for(int i = 0; i < iter; i++){
    // Pick a random number in [2 to (number-2)]
    int a = 2 + rand() % (number - 2);
    //checking if composite
    if (compositeCheck(a, d, number, k)){
      return 0;
    }
  }    
  return 1;
}
// [[Rcpp::export]]
int generatePrime(int n){
  int number, p;
  do{
    number = NumberGenerator(n);
  }while(PrimeCheck(number) == 0);
  p = number;
  //do{
  //  number = NumberGenerator(n);
  //}while(PrimeCheck(number) == 0 || number == p);
  //q = number;
  return p;
}

// [[Rcpp::export]]
NumericVector messageEncrypt(const NumericVector plaintext, int e, int n){
  NumericVector ciphertext = clone(plaintext);
  for(int index = 0; index < plaintext.size(); index++){
    //char character = Rcpp::as<char>(ciphertext[index]);
    //convert char to ascii and encrypt ascii value
    int encryptedValue = modExp((int)ciphertext[index], e, n);
    //convert value to char
    ciphertext[index] = encryptedValue;
  }
  return ciphertext;
}
// [[Rcpp::export]]
NumericVector messageDecrypt(const NumericVector ciphertext, int d, int n){
  NumericVector plaintext = clone(ciphertext);
  for(int index = 0; index < ciphertext.size(); index++){
    //char character = Rcpp::as<char>(plaintext[index]);
    //convert char to ascii and decrypt ascii value
    int decryptedValue = modExp((int)plaintext[index], d, n);
    //convert value to char
    plaintext[index] = decryptedValue;
  }
  return plaintext;
}
// [[Rcpp::export]]
List keyGenerator(int p, int q){
  int e, d;
  //check uniqueness
  if (p == q){
    cout << "please try again with two unique prime numbers" << endl;
    return 0;
  }
  int n = p * q;
  int m = (p - 1) * (q - 1);
  //finding e in which is coprime with m
  do{
    e = 1 + ( std::rand() % (m));
  }while(gcd(e, m) != 1);
  //finding d in which is the modular inverse of e
  do{
    d = 1 + ( std::rand() % (m));
  } while ((d * e) % m != 1);
  return List::create(n, e, d);
}
/*void testRSA(CharacterVector message = {"H", "e", "l", "l", "o", " ", "W", "o", "r", "l", "d", "!"}, int testp = 2, int testq = 7, int teste = 5, int testd = 11){
  List keys = keyGenerator(testp, testq);
  int n = keys[0];
  int e = keys[1];
  int d = keys[2];
  cout << "(n, e, d) = (" << n << ", " << e << ", " << d << ")\n";
  CharacterVector encodedMessage = messageEncrypt(message, e, n);
  cout <<"encoded message: "<< encodedMessage;
  CharacterVector decodedMessage = messageDecrypt(encodedMessage, d, n);
  cout <<"decoded message: "<< decodedMessage;
  
  CharacterVector encodedMessage2 = messageEncrypt(message, teste, n);
  cout <<"encoded message2: "<< encodedMessage2;
  CharacterVector decodedMessage2 = messageDecrypt(encodedMessage, testd, n);
  cout <<"decoded message2: "<< decodedMessage2;
}*/


