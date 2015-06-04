# Mercury
Mercury programming language
The "primality" and "primes" modules and related test platforms implement and use a Miller-Rabin primality test to
explore prime numbers. The "wip" version uses Mercury's Integer Library for arbitrary precision integers.
  1. The "ap" module provides memoized integers of arbitrary precision and two math functions not provided by the
     Mercury Integer Library for use by "primality" and "primes".
  2. The "primality" module implements the Miller-Rabin primality test using pred isPrime/1.
  3. The "primes" module implements several functions useful in exploring prime numbers, these include:
     a. primeFactors/1 that returns a list of the prime factors of a composite number;
     b. primes/2 that returns a list of all prime numbers from a lower limit through an upper limit; and
     c. pred prime/1 that uses trial division and the Miller-Rabin primality test to determine if an integer is prime.
