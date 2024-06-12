fun binomial(n, k) =
    let
        fun binomial_tail(n, k, num, den) =
            if k = 0 then
                num div den
            else if k > n then
                0
            else
                binomial_tail(n - 1, k - 1, num * n, den * k)
    in
        binomial_tail(n, k, 1, 1)
    end

fun binomial2(n, k) =
    let
        fun pascalTriangleRow 0 = [1]
            | pascalTriangleRow r =
            let
                val prevRow = pascalTriangleRow (r - 1)
                fun nextElement (_, []) = [1]
                    | nextElement ([], _) = [1]
                    | nextElement (x::xs, y::ys) = (x + y) :: nextElement(xs, ys)
            in
                1 :: nextElement(prevRow, tl prevRow) @ [0]
            end;

        val triangle = List.tabulate(n + 1, pascalTriangleRow);
    in
        List.nth(List.nth(triangle, n), k)
    end;

fun merge(xs, []) = xs
  | merge([], ys) = ys
  | merge(x::xs, y::ys) =
    if x <= y then
        x :: merge(xs, y::ys)
    else
        y :: merge(x::xs, ys)

fun mergeSort xs =
    if null xs orelse null (tl xs) then
        xs
    else
        let
            val len = length xs
            val half = len div 2
            val left = List.take(xs, half)
            val right = List.drop(xs, half)
        in
            merge(mergeSort left, mergeSort right)
        end
    
fun extendedGcd(a, b) =
    if b = 0 then
        (a, 1, 0)
    else
        let
            val(g, x1, y1) = extendedGcd(b, a mod b)
        in
            (g, y1, x1 - (a div b) * y1)
        end

fun de(a, b) =
    let
        val(g, x, y) = extendedGcd(a, b)
    in
        (g, x, y)
    end

fun factorize(d, n) =
    if d * d > n then
        [n]
    else if n mod d = 0 then
        d :: factorize(d, n div d)
    else
        factorize(d + 1, n)

fun primeFactors(n) =
    if n <= 1 then
        []
    else
        factorize(2, n)

fun gcd(a, b) =
    if b = 0 then
        a
    else
        gcd(b, a mod b)

fun totient n =
    let
        fun countCoprimes(k, result) =
            if k = n then
                result
            else if gcd(n, k) = 1 then
                countCoprimes(k + 1, result + 1)
            else
                countCoprimes(k + 1, result)
    in
        countCoprimes(1, 0)
    end

fun removeDuplicates [] = []
    | removeDuplicates (x::xs) = x::removeDuplicates(List.filter (fn y => y <> x) xs)

fun totient2(n) =
    if n <= 1 then
        0
    else
        let
            val factors = removeDuplicates(primeFactors(n))
            val phi = Real.round (Real.fromInt n * List.foldl (fn (p, acc) => acc * (1.0 - 1.0 / Real.fromInt p)) 1.0 factors)
        in
            phi
        end
    
fun sieve [] = []
    | sieve (p::xs) =
    p :: sieve (List.filter (fn x => x mod p <> 0) xs)

fun primes(n) =
    let
        val sieveList = List.tabulate (n - 1, fn i => i + 2)
    in
        sieve sieveList
    end

val bin1 = binomial(10, 3)
val bin2 = binomial2(10, 3)
val sortedList = mergeSort[4, 2, 7, 1, 5, 3, 6, 8]
val eq = de(56, 15)
val fact = primeFactors(50)
val primeList = primes(50)
val euler1 = totient(100)
val euler2 = totient2(100);