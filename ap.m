%----------------------------------------------------------------------%
% language: Mercury
% module:   ap
% file:     ap.m
% revised:  23 MAY 2015 @ 10:00AM
% purpose:  provides integer constants for primes, primality
%----------------------------------------------------------------------%
%%% The Mercury library Integer provides two specific integers of
%%% arbitrary precision: integer.zero and integer.one. This module
%%% provides additional integers of arbitrary precision used in the 
%%% module primality. To use this module's representation for the 
%%% integer 1000, for example, one would call ap.n1000.

:- module ap.

:- interface.

:- import_module integer.

:- func n2 = integer.integer.             % name is "nxxxx"
:- func n3 = integer.integer.
:- func n5 = integer.integer.
:- func n7 = integer.integer.
:- func n11 = integer.integer.
:- func n13 = integer.integer.
:- func n17 = integer.integer.
:- func n23 = integer.integer.
:- func n31 = integer.integer.
:- func n61 = integer.integer.
:- func n73 = integer.integer.
:- func n1000 = integer.integer.
% :- func n7993 = integer.integer.
:- func n8000 = integer.integer.
:- func n1373653 = integer.integer.
:- func n1662803 = integer.integer.
:- func n9080191 = integer.integer.
:- func n25326001 = integer.integer.
% :- func n63888049 = integer.integer.
:- func n3215031751 = integer.integer.
:- func n4759123141 = integer.integer.
:- func n1122004669633 = integer.integer.
:- func n2152302898747 = integer.integer.
:- func n3474749660383 = integer.integer.
:- func n341550071728321 = integer.integer.
:- func n1442695040888963407 = integer.integer.
:- func n6364136223846793005 = integer.integer.

:- func powm(integer, integer, integer) = integer.	
:- func gcd(integer, integer) = integer.

:- implementation.

:- import_module require, string.

:- pragma memo(n2/0).
:- pragma memo(n3/0).
:- pragma memo(n5/0).
:- pragma memo(n7/0).
:- pragma memo(n11/0).
:- pragma memo(n13/0).
:- pragma memo(n17/0).
:- pragma memo(n23/0).
:- pragma memo(n31/0).
:- pragma memo(n61/0).
:- pragma memo(n73/0).
:- pragma memo(n1000/0).
% :- pragma memo(n7993/0).
:- pragma memo(n8000/0).
:- pragma memo(n1373653/0).
:- pragma memo(n1662803/0).
:- pragma memo(n9080191/0).
:- pragma memo(n25326001/0).
% :- pragma memo(n63888049/0).
:- pragma memo(n3215031751/0).
:- pragma memo(n4759123141/0).
:- pragma memo(n1122004669633/0).
:- pragma memo(n2152302898747/0).
:- pragma memo(n3474749660383/0).
:- pragma memo(n341550071728321/0).
:- pragma memo(n1442695040888963407/0).
:- pragma memo(n6364136223846793005/0).

% name = integer.det_from_string("xxxx").    for bigints > 2.15e+9
% name = integer.integer(N).                 for bigints < 2.15e+9
% interger.integer(N) fails to work on some PPC machines for N > 2.15e+9
% the string solution works on both x86-64 and PPC architectures, as
% seen below.

n2 = integer.integer(2).
n3 = integer.integer(3).
n5 = integer.integer(5).
n7 = integer.integer(7).
n11 = integer.integer(11).
n13 = integer.integer(13).
n17 = integer.integer(17).
n23 = integer.integer(23).
n31 = integer.integer(31).
n61 = integer.integer(61).
n73 = integer.integer(73).
n1000 = integer.integer(1000).
% n7993 = integer.integer(7993).
n8000 = integer.integer(8000).
n1373653 = integer.integer(1373653).
n1662803 = integer.integer(1662803).
n9080191 = integer.integer(9080191).
n25326001 = integer.integer(25326001).
% n63888049 = integer.integer(63888049).
n3215031751 = integer.det_from_string("3215031751").
n4759123141 = integer.det_from_string("4759123141").
n1122004669633 = integer.det_from_string("1122004669633").
n2152302898747 = integer.det_from_string("2152302898747").
n3474749660383 = integer.det_from_string("3474749660383").
n341550071728321 = integer.det_from_string("341550071728321").
n1442695040888963407 = integer.det_from_string("1442695040888963407").
n6364136223846793005 = integer.det_from_string("6364136223846793005").

%----------------------------------------------------------------------%

	           %% computes (A^D) mod N

powm(A, D, N) =	Res :-		
	( 
	  D = integer.zero ->
	      Res = integer.one
	;
	  (D mod ap.n2) = integer.zero -> 
	      Res = ((integer.pow(powm(A, (D div ap.n2), N), ap.n2)) mod N)
	;
	      Res = ((A * powm(A, D - integer.one, N)) mod N)
	).

%----------------------------------------------------------------------%
	%% gcd/2 finds the greatest common denominator of integers, A, B.

gcd(A, B) = (if B = integer.zero then A else gcd(B, A mod B)).

%----------------------------------------------------------------------%
:- end_module ap.
