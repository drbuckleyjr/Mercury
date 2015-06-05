%----------------------------------------------------------------------%
% language: Mercury
% module:   test_isPrime_wip
% file:     test_isPrime_wip.m
% revised:  24 MAY 2015 @ 10:00AM
% purpose:  test predicate isPrime/1
%----------------------------------------------------------------------%

:- module test_isPrime_wip.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------%

:- implementation.

:- import_module bool, char, int, integer, list, math, require, string.
:- import_module primality_wip.

%----------------------------------------------------------------------%
  
main(!IO) :-
    command_line_arguments(Args, !IO),
    filter(is_all_digits, Args, CleanArgs),
    Arg1 = list.det_index0(CleanArgs, 0),
    M = integer.det_from_string(Arg1),
  %--------------------------------------------------------%
  %          TEST THE isPrime/1 PREDICATE
  %          $ ./test_isPrime_mpm M
  %--------------------------------------------------------%
    ( isPrime(M) ->
        io.format("   isPrime(%s) = true.\n", 
                            [s(integer.to_string(M))], !IO)
    ;
        io.format("   isPrime(%s) = false.\n", 
                            [s(integer.to_string(M))], !IO)
    ).

%----------------------------------------------------------------------%
:- end_module test_isPrime_wip.

% Example output

% isPrime(4547337172376300111955330758342147474062293202868155909489) = true.

% isPrime(4547337172376300111955330758342147474062293202868155909393) = false.
