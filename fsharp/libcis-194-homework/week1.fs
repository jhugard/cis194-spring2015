namespace cis_194_homework

module week1 =

    let lastDigit n =
        n % 10I
       
    let dropLastDigit n =
        n / 10I

    let rec toRevDigits = function
        | n when n<=0I ->
            []
        | n ->
            lastDigit n :: (toRevDigits (dropLastDigit n))
   
    let toDigits = List.rev << toRevDigits

    let rec doubleEveryOther = function
        | []           -> []
        | [a]          -> [a]
        | a :: b :: tl -> a :: (b+b) :: doubleEveryOther tl


    // In Haskell, this works:
    //
    //    sumDigits = foldr ((+) . sum . toDigits) 0
    //
    // But in F#, the following does _not_ work:
    //
    //    let sumDigits' = List.fold ((+) << List.sum << toRevDigits) 0I
    //
    // In the following working function, there must be a better way that
    // can go totally point free ala Haskel.  But with List.fold, the
    // accumulator is passed to RevDigits rather than the list element.

    let sumDigits xs = List.foldBack ((+) << List.sum << toRevDigits) xs 0I


    let lunh n =
        0I = ((sumDigits << doubleEveryOther << toRevDigits) n % 10I)

    let rec hanoi n (a:string) (b:string) (c:string) : (string*string) list =
        match n with
        | 0 -> []
        | 1 -> [(a,c)]
        | n -> List.concat [
                hanoi (n-1) a c b
                hanoi    1  a b c
                hanoi (n-1) b a c 
                ]
