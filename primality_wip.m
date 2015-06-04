%----------------------------------------------------------------------%
% language: Mercury
% module:   primality_wip
% file:     primality_wip.m
% revised:  24 MAY 2015 @ 9:55AM
% purpose:  rapidly tests primality of integer N
%----------------------------------------------------------------------%

:- module primality_wip.

:- interface.

:- import_module integer.
:- pred isPrime(integer::in) is semidet.

%----------------------------------------------------------------------%

:- implementation.

:- import_module bool, int, list, math, require, string.
:- import_module ap.
:- pred isPrime_helper(integer::in, list(integer)::in) is semidet.
:- pred outer_loop(integer::in, list(integer)::in, 
                         integer::in, integer::in) is semidet.
:- pred inner_loop(integer::in, integer::in, 
                         integer::in, integer::in) is semidet.
:- func deterministic_witnesses(integer) = list(integer).
:- func random_witnesses(integer, int) = list(integer).	
:- func rw_loop(integer, integer, integer, 
                   integer, int, list(integer)) = list(integer).
:- func find_ds(integer) = list(integer).
:- func find_ds_loop(integer, integer) = list(integer).

%----------------------------------------------------------------------%

isPrime(N) :-
    ( N < n2 -> false
    ;
      N = n2 -> true
    ;
      N = n3 -> true
      
    ;         %% N > 3, N even 
	  N > n3,
	      (N rem n2) = zero -> false
	      
	;         %% N > 3, odd, < 3.415e+14: use deterministic Wits
      N > n3,
          (N rem n2) = one,
          N < n341550071728321,
	      DList = deterministic_witnesses(N) ->
	               isPrime_helper(N, DList)
	               
	;         %% N is odd >= 3.415e+14: use probabilistic Wits
	  N > n3,
	      (N rem n2) = one,
	      N >= n341550071728321,
	      RList = random_witnesses(N, 20),  %% get 20 Wits
	      isPrime_helper(N, RList)
	).

%----------------------------------------------------------------------%

isPrime_helper(N, As) :-
    find_ds(N) = [D|T],
    T = [S|_],
    outer_loop(N, As, D, S).


outer_loop(N, As, D, S) :-
    As = [A|At],
    Base = powm(A, D, N),
    ( not(inner_loop(Base, N, zero, S)) -> false
    ;
      At = [] -> true
    ;
          outer_loop(N, At, D, S)
    ).   


inner_loop(Base, N, Loop, S) :-
    Next_Base = (Base * Base) rem N,
    Next_Loop = Loop + one,
    ( Loop = zero ->
          ( 
            Base = one -> true
          ;
            Base = N - one -> true
          ;
            Next_Loop = S -> false
          ;
                inner_loop(Next_Base, N, Next_Loop, S)
          )
    ;
      Base = N - one -> true
    ;
      Next_Loop = S -> false
    ;
          inner_loop(Next_Base, N, Next_Loop, S)
    ).

%----------------------------------------------------------------------%

deterministic_witnesses(N) = Res :-
    ( N < n1373653 ->       
          Res = [n2, n3]
    ;
      N < n9080191 ->       
          Res = [n31, n73]
    ;
      N < n25326001 ->      
          Res = [n2, n3, n5]
    ;
      N < n3215031751 ->    
          Res = [n2, n3, n5, n7]
    ;
      N < n4759123141 ->    
          Res = [n2, n7, n61]
    ;
      N < n1122004669633 ->
          Res = [n2, n13, n23, n1662803]
    ;
      N < n2152302898747 -> 
          Res = [n2, n3, n5, n7, n11]
    ;
      N < n3474749660383 ->
          Res = [n2, n3, n5, n7, n11, n13]
    ;
          Res = [n2, n3, n5, n7, n11, n13, n17]
    ).

%----------------------------------------------------------------------%

random_witnesses(N, K) =
  rw_loop(N, n6364136223846793005, 
             n1442695040888963407, N - n2, K, []).
	
					
rw_loop(X, A, B, C, K, L) = Res :-
    (X1 = (((X * A) + B) rem C) + one),
	( K > 0 -> Res = rw_loop(X1, A, B, C, K-1, [X1|L])
	;
	           Res = L
	).

%----------------------------------------------------------------------%

find_ds(N) = find_ds_loop(N - one, zero).


find_ds_loop(D, S) = Res :-
    NewD = D // n2,
	NewS = S + one,
	( D rem n2 = zero ->
	      Res = find_ds_loop(NewD, NewS)
	;
	      Res = [D, S]
	).

%----------------------------------------------------------------------%
:- end_module primality_wip.
