module Fuzz.Extra exposing (oneOf, pick, someOf)

import Fuzz exposing (Fuzzer, andThen, intRange)
import Random.Pcg as R
import Result exposing (Result(..))
import Shrink


-- all I want for christmas:


oneOf : List a -> Fuzzer a
oneOf list =
    case list of
        [] ->
            Err "Fuzz.Extra.oneOf was given an empty list."

        x :: _ ->
            Fuzz.intRange 0 (List.length list)
                |> Fuzz.map (\pos -> listGet pos list |> Maybe.withDefault x)


pick : Int -> List a -> Fuzzer (List a)
pick n list =
    if n > List.length list then
        Err <|
            "Fuzz.Extra.pick can not give "
                ++ toString n
                ++ " elements of an "
                ++ (List.length list |> toString)
                ++ " elments long list."
    else
        shuffle list
            |> R.map (List.take n)
            |> flip Fuzz.custom Shrink.noShrink


someOf : List a -> Fuzzer (List a)
someOf list =
    intRange 0 (List.length list) |> andThen (\toPick -> pick toPick list)



--helpers


listGet : Int -> List a -> Maybe a
listGet pos l =
    List.drop (pos - 1) l
        |> List.head


shuffle : List a -> R.Generator (List a)
shuffle list =
    let
        rec : List a -> List a -> R.Generator (List a)
        rec todo acc =
            R.andThen
                (\pos ->
                    case todo of
                        [] ->
                            R.constant acc

                        xs ->
                            let
                                left =
                                    List.take pos xs

                                rightWithElement =
                                    List.drop pos xs

                                newTodo =
                                    left ++ List.drop 1 rightWithElement

                                newAcc =
                                    (rightWithElement
                                        |> List.head
                                        |> Maybe.map (\a -> [ a ])
                                        |> Maybe.withDefault []
                                    )
                                        ++ acc
                            in
                            rec newTodo newAcc
                )
                (R.int 0 (List.length todo - 1))
    in
    rec list []



-- /** A generator that picks a random number of elements from a list */
--  def someOf[T](l: Iterable[T]) = choose(0,l.size).flatMap(pick(_,l))
--/** Picks a random generator from a list */
--  def oneOf[T](g0: Gen[T], g1: Gen[T], gn: Gen[T]*): Gen[T] = {
--  /** A generator that returns `Some(T)` */
--  def some[T](g: Gen[T]): Gen[Option[T]] =
-- /** Generates a non-empty list of random length. The maximum length depends
--   *  on the size parameter. This method is equal to calling
--   *  `nonEmptyContainerOf[List,T](g)`. */
--  def nonEmptyListOf[T](g: => Gen[T]) = nonEmptyBuildableOf[List[T],T](g)
--
--  /** Generates a list of the given length. This method is equal to calling
--   *  `containerOfN[List,T](n,g)`. */
--  def listOfN[T](n: Int, g: Gen[T]) = buildableOfN[List[T],T](n,g)
--  /** A generator that picks a given number of elements from a list, randomly */
--  def pick[T](n: Int, l: Iterable[T]): Gen[Seq[T]] = {
--
--  /** Generates a numerical character */
--  def numChar: Gen[Char] = choose(48.toChar, 57.toChar)
--
--  /** Generates an upper-case alpha character */
--  def alphaUpperChar: Gen[Char] = choose(65.toChar, 90.toChar)
--
--  /** Generates a lower-case alpha character */
--  def alphaLowerChar: Gen[Char] = choose(97.toChar, 122.toChar)
--
--  /** Generates an alpha character */
--  def alphaChar = frequency((1,alphaUpperChar), (9,alphaLowerChar))
--
--  /** Generates an alphanumerical character */
--  def alphaNumChar = frequency((1,numChar), (9,alphaChar))
--
--/** Generates a string of digits */
--  def numStr: Gen[String] =
--    listOf(numChar).map(_.mkString)
--
--  /** Generates a string of upper-case alpha characters */
--  def alphaUpperStr: Gen[String] =
--    listOf(alphaUpperChar).map(_.mkString)
--
--  /** Generates a string of lower-case alpha characters */
--  def alphaLowerStr: Gen[String] =
--      listOf(alphaLowerChar).map(_.mkString)
--
--  /** Generates a string of alpha characters */
--  def alphaStr: Gen[String] =
--    listOf(alphaChar).map(_.mkString)
--
--  /** Generates a string of alphanumerical characters */
--  def alphaNumStr: Gen[String] =
--    listOf(alphaNumChar).map(_.mkString)
--
--/** Generates positive numbers of uniform distribution, with an
--   *  upper bound of the generation size parameter. */
--  def posNum[T](implicit num: Numeric[T], c: Choose[T]): Gen[T] = {
--    import num._
--    sized(n => c.choose(one, max(fromInt(n), one)))
--  }
--
--  /** Generates negative numbers of uniform distribution, with an
--   *  lower bound of the negated generation size parameter. */
--  def negNum[T](implicit num: Numeric[T], c: Choose[T]): Gen[T] = {
--    import num._
--    sized(n => c.choose(min(-fromInt(n), -one), -one))
--  }
