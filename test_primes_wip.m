%----------------------------------------------------------------------%
% module: test_primes_wip
% file: test_primes_wip.m
% version: 15 MAY 2015 @ 10:00AM
% purpose: tests the function primes/2
%----------------------------------------------------------------------%
:- module test_primes_wip.

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
    Arg2 = list.det_index0(CleanArgs, 1),
    M = integer.det_from_string(Arg1),
    N = integer.det_from_string(Arg2),
  %--------------------------------------------------------%
  % TEST THE PRIMES FUNCTION
  % $ ./test_primes_wip M N
  %--------------------------------------------------------%
    P = primes(M, N),    %% primes(M,N) is det.
    io.format("   primes(%s, %s) = [", [s(integer.to_string(M)), s(integer.to_string(N))],!IO),
    io.write_string(string.join_list(", ", map(integer.to_string, P)), !IO),
    io.write_string("].\n", !IO).

%----------------------------------------------------------------------%
:- end_module test_primes_wip.

% is_prime(4547337172376300111955330758342147474062293202868155909489) = true.

% is_prime(4547337172376300111955330758342147474062293202868155909393) = false.

