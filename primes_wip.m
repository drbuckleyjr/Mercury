%----------------------------------------------------------------------%
% language: Mercury
% module:   primes_wip
% file:     primes_wip.m
% revised:  24 MAY 2015 @ 10:00AM
% purpose:  provides predicate prime/1, and functions primes/2, 
%               primeFactors/1, rho/1
%
%----------------------------------------------------------------------%
:- module primes_wip.

:- interface.

:- import_module integer, list.
:- pred prime(integer:: in) is semidet.
:- func primes(integer, integer) = list(integer) is det.
:- func primeFactors(integer) = list(integer) is semidet.
:- func rho(integer) = integer is semidet.

%----------------------------------------------------------------------%

:- implementation.

:- import_module bool, char, int, math, require, string.
:- import_module ap, primality_wip.	
:- pred primE(integer::in, integer::in) is semidet.	
:- func primEs(integer, integer, list(integer)) = list(integer) is det.    
:- func prime_factors(integer, list(integer), list(integer)) = 
                                              list(integer) is semidet.	
:- func f(integer, integer, integer) = integer.    
:- func rho1(integer, integer, integer, integer) = integer is semidet.

%----------------------------------------------------------------------%
	%% prime/1 returns true if N is prime, else fails
	
prime(N) :-
	( N < n1000 ->
	      primE(N, n2)
	;
	      isPrime(N)
	).


	%% primE/2 provides a loop for prime/1

primE(N, K) :-
	( N < n2 -> false
	;
	  N = n2 -> true
	;
	  N = n3 -> true
	;
	  N =< K -> true
	;
	  (N rem K) = zero -> false
	;
	  K = n2 -> primE(N, n3)
	;
	            primE(N, K + n2)
	).              

%----------------------------------------------------------------------%
	%% primes/2 receives two integers M, N, and returns a list
	%% of all primes from M to N, inclusive. If there are no primes
	%% in the interval, primes/2 returns the empty list, [].


primes(M, N) = Res :- 
    Res = primEs(M, N, []).


	%% primEs/4 provides a loop for primes/3

primEs(M, N, L) = Res :-
	( M > N ->
	      list.reverse(L, P),
	      Res = P
	;
	  M < n2 ->
	      Res = primEs(n2, N, L)
	;
	  M = n2 ->
	      Res = primEs(n3, N, [n2|L])
	;
	  prime(M) ->
	      Res = primEs(M + n2, N, [M|L])
	;
	  (M rem n2) = zero ->
	     Res = primEs(M + one, N, L)
	;
	     Res = primEs(M + n2, N, L)
	).

%----------------------------------------------------------------------%
	%% primeFactors/1 returns a list of all prime factors of N

primeFactors(N) = Res :-
    P = primes(n2, n8000), %% list of all primes from 2 through 8000
    Res = prime_factors(N, P, []).


prime_factors(N, P, L) = Res :-
    ( prime(N) ->
          Res = [N|L]
    ;
      P \= [] ->
          [H|T] = P,
          ( N rem H = zero ->
                Res = prime_factors(N // H, P, [H|L])
          ;
                Res = prime_factors(N, T, L)
          )
    ;
      P = [] ->
          Z = rho(N),
          Res = prime_factors(N // Z, [], [Z|L])
    ;
          Res = prime_factors(N, [], L)
    ).
    
%----------------------------------------------------------------------%
	%% f/3 generates a pseudo-random number

f(X, R, N) = ((X * X) +R) rem N.
	
%----------------------------------------------------------------------%
	%% rho/1 receives a composite integer, N, and returns
	%% a factor, P, of N. When N is prime, rho returns N.
%:- func rho(gmp_int) = gmp_int is semidet.

rho(N) = Res :-
    R = n7,
    A = n2,
    X = f(A, R, N),
    Y = f(X, R, N),
    Res = rho1(N, R, X, Y).


    %% rho1/4 is a fast loop for factoring composite, N

rho1(N, R, X, Y) = Res :-
    XN = f(X, R, N),
    B  = f(Y, R, N),
    YN = f(B, R, N),
    Z = gcd(N, abs(X - Y)),
    ( Z = one ->
          Res = rho1(N, R, XN, YN)
    ;
      prime(Z),
          Res = Z
    ). 

%----------------------------------------------------------------------%
:- end_module primes_wip.
