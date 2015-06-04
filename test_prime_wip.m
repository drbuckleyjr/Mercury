%----------------------------------------------------------------------%
% module: test_prime_wip
% file: test_prime_wip.m
% version: 17 MAY 2015 @ 4:10PM
% purpose: tests the function prime/1
%----------------------------------------------------------------------%
:- module test_prime_wip.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------%

:- implementation.

:- import_module bool, char, int, integer, list, math, require, string. 
:- import_module primes_wip.

%----------------------------------------------------------------------%
main(!IO) :-
    command_line_arguments(Args, !IO),
    filter(is_all_digits, Args, CleanArgs),
    Arg1 = list.det_index0(CleanArgs, 0),
%    Arg2 = list.det_index0(CleanArgs, 1),
    M = integer.det_from_string(Arg1),
%    N = integer.det_from_string(Arg2),
  %--------------------------------------------------------%
  % TEST THE PRIME FUNCTION
  % $ ./test_prime M
  %--------------------------------------------------------%
	( prime(M) -> 
	      io.format("   prime(%s) = true.\n", 
	                     [s(integer.to_string(M))], !IO)
	;
	      io.format("   prime(%s) = false.\n", 
	                     [s(integer.to_string(M))], !IO)
	).

%----------------------------------------------------------------------%
:- end_module test_prime_wip.
