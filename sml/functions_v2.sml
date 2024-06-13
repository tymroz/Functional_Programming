fun binomial (n, k) =
    if k = 0 orelse k = n then 1
    else binomial (n-1, k-1) + binomial (n-1, k)

fun pascalRow [] = [1]
  | pascalRow row = [1] @ (ListPair.mapOp (op +) (row, tl row)) @ [1]

fun binomial2 (n, k) =
    let
        fun iterate 0 row = row
          | iterate m row = iterate (m-1) (pascalRow row)
    in
        nth (iterate n [1]) k
    end

fun mergesort [] = []
  | mergesort [x] = [x]
  | mergesort xs =
    let
      val (left, right) = List.takeDrop (length xs div 2, xs)
      fun merge ([], ys) = ys
        | merge (xs, []) = xs
        | merge (x::xs, y::ys) =
            if x <= y then x :: merge (xs, y::ys)
            else y :: merge (x::xs, ys)
    in
      merge (mergesort left, mergesort right)
    end

fun gcdExt (0, b) = (b, 0, 1)
  | gcdExt (a, b) =
    let
      val (g, x, y) = gcdExt (b mod a, a)
    in
      (g, y - (b div a) * x, x)
    end

fun de (a, b) = gcdExt (a, b)

fun prime_factors n =
    let
        fun factors (1, _) = []
          | factors (n, f) =
            if n mod f = 0 then f :: factors (n div f, f)
            else factors (n, f + 1)
    in
        factors (n, 2)
    end

fun gcd (a, 0) = a
  | gcd (a, b) = gcd (b, a mod b)

fun totient n = List.length (List.filter (fn x => gcd (x, n) = 1) (List.tabulate (n, fn i => i + 1)))

fun count_factors (p, n) =
    if n mod p = 0 then 1 + count_factors (p, n div p) else 0

fun totient2 n =
    let
        val factors = List.map (fn p => (p, count_factors (p, n))) (List.uniq (prime_factors n))
        fun phi p m = (p - 1) * IntInf.pow (p, m - 1)
    in
        List.foldl (fn ((p, m), acc) => acc * phi p m) 1 factors
    end

fun sieve [] = []
  | sieve (p::xs) = p :: sieve (List.filter (fn x => x mod p <> 0) xs)

fun primes n = sieve (List.tabulate (n-1, fn i => i+2))

val bin1 = binomial(10, 3)
val bin2 = binomial2(10, 3)
val sortedList = mergeSort[4, 2, 7, 1, 5, 3, 6, 8]
val eq = de(56, 15)
val fact = primeFactors(50)
val primeList = primes(50)
val euler1 = totient(100)
val euler2 = totient2(100);