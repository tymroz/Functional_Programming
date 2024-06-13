% mergesort(+X, -Y)
mergesort([], []).
mergesort([X], [X]).
mergesort(List, Sorted) :-
    split(List, Left, Right),
    mergesort(Left, SortedLeft),
    mergesort(Right, SortedRight),
    merge(SortedLeft, SortedRight, Sorted).

split([], [], []).
split([X], [X], []).
split([X, Y | Rest], [X | Left], [Y | Right]) :-
    split(Rest, Left, Right).

merge([], Right, Right).
merge(Left, [], Left).
merge([X | Left], [Y | Right], [X | Merged]) :-
    X =< Y,
    merge(Left, [Y | Right], Merged).
merge([X | Left], [Y | Right], [Y | Merged]) :-
    X > Y,
    merge([X | Left], Right, Merged).

% de(+A, +B, -X, -Y, -Z)
de(A, B, X, Y, G) :-
    gcd_ext(A, B, G, X, Y).

gcd_ext(A, 0, A, 1, 0).
gcd_ext(A, B, G, X, Y) :-
    B \= 0,
    Q is A // B,
    R is A mod B,
    gcd_ext(B, R, G, X1, Y1),
    X is Y1,
    Y is X1 - Q * Y1.

% prime_factors(+N, -X)
prime_factors(N, Factors) :-
    prime_factors(N, 2, Factors).

prime_factors(1, _, []).
prime_factors(N, F, [F | Factors]) :-
    0 is N mod F,
    N1 is N // F,
    prime_factors(N1, F, Factors).
prime_factors(N, F, Factors) :-
    F * F < N,
    next_factor(F, NF),
    prime_factors(N, NF, Factors).

next_factor(2, 3).
next_factor(F, NF) :-
    F > 2,
    NF is F + 2.

% totient(+N, -T)
totient(N, T) :-
    findall(X, (between(1, N, X), gcd(N, X, 1)), RelativelyPrimes),
    length(RelativelyPrimes, T).

gcd(A, 0, A).
gcd(A, B, G) :-
    B \= 0,
    R is A mod B,
    gcd(B, R, G).

% primes(+N, -X)
primes(N, Primes) :-
    findall(X, (between(2, N, X), is_prime(X)), Primes).

is_prime(2).
is_prime(3).
is_prime(N) :-
    N > 3,
    N mod 2 =\= 0,
    \+ has_factor(N, 3).

has_factor(N, F) :-
    N mod F =:= 0.
has_factor(N, F) :-
    F * F < N,
    F2 is F + 2,
    has_factor(N, F2).

between(Low, High, Low) :-
    Low =< High.
between(Low, High, Value) :-
    Low < High,
    NextLow is Low + 1,
    between(NextLow, High, Value).
