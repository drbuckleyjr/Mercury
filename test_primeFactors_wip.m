%----------------------------------------------------------------------%
% module: test_primeFactors_wip
% file: test_primeFactors_wip.m
% version: 15 MAY 2015 @ 10:00AM
% purpose: tests the function primeFactors/1
%----------------------------------------------------------------------%
:- module test_primeFactors_wip.

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
%   Arg2 = list.det_index0(CleanArgs, 1),
    M = integer.det_from_string(Arg1),
%   N = integer.det_from_string(Arg2),
  %--------------------------------------------------------%
  % TEST THE PRIMEFACTORS PREDICATE
  % $ ./test_primeFactors M
  %--------------------------------------------------------%
	( P = primeFactors(M) ->
	      io.format("   primeFactors(%s) = [",
							[s(integer.to_string(M))],!IO),
	      io.write_string(string.join_list(", ", 
							 map(integer.to_string, P)), !IO),
	      io.write_string("].\n", !IO)
	;
	      io.format("   primeFactors(%s) failed.\n", 
	                     [s(integer.to_string(M))],!IO)
	).

%----------------------------------------------------------------------%
:- end_module test_primeFactors_wip.
