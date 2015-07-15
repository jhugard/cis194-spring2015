module testcis_194_homework

open System
open NUnit.Framework

open cis_194_homework
open week1

let applyTest f (v,expected) =
    let actual = f v
    Assert.AreEqual(expected :> obj, actual :> obj)

let runTests f =
    List.iter (applyTest f)

[<TestFixture>]
type Ex1_Tests() = 

    [<Test>]
    member x.LastDigitTests() =
        [
            (123I,3I)
            (1234I,4I)
            (5I,5I)
            (10I,0I)
            (0I,0I)
            ]
        |> runTests lastDigit

    [<Test>]
    member x.DropLastDigitTests() =
        [
            (123I,12I)
            (1234I,123I)
            (5I,0I)
            (10I,1I)
            (0I,0I)
            ]
        |> runTests dropLastDigit


[<TestFixture>]
type Ex2_Tests() = 

    [<Test>]
    member x.toRevDigitsTests() =
        [
            (1234I,[4I;3I;2I;1I])
            (0I,[])
            (-17I,[])
        ]
        |> runTests toRevDigits

    [<Test>]
    member x.toDigitsTests() =
        [
            (1234I,[1I;2I;3I;4I])
            (0I,[])
            (-17I,[])
        ]
        |> runTests toDigits

[<TestFixture>]
type Ex3_Tests() =

    [<Test>]
    member x.doubleEveryOtherTests() =
        [
            ([4I; 9I; 5I; 5I], [4I; 18I; 5I; 10I])
            ([0I; 0I], [0I; 0I])
        ]
        |> runTests doubleEveryOther

[<TestFixture>]
type Ex4_Tests() =

    [<Test>]
    member x.sumDigitsTests() =
        [
            ([10I; 5I; 18I; 4I], 19I)
        ]
        |> runTests sumDigits
 
[<TestFixture>]
type Ex5_Tests() =

    [<Test>]
    member x.lunhTests() =
        [
            (5594589764218858I, true)
            (1234567898765432I, false)
        ]
        |> runTests lunh

[<TestFixture>]
type Ex6_Tests() =

    [<Test>]
    member x.hanoiTests() =
        let a,b,c = "a", "b", "c"
        let tests =
            [
                ((2,a,b,c), [(a,b); (a,c); (b,c)]);
                ((3,a,b,c),
                    [
                        (a,c); (a,b); (c,b)
                        (a,c)
                        (b,a); (b,c); (a,c)
                    ]
                )
            ]

        let hanoiTuppled (n,a,b,c) = hanoi n a b c

        tests
        |> runTests hanoiTuppled
