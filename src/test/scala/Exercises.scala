import org.scalacheck.Prop._
import org.scalacheck.Properties

import Exercises._

object CourseSpecification extends Properties("Course") {
  property("ex01 - add") = 
    forAll((x: Int, y: Int) => add(x, y) == x + y)

  property("ex02 - sum") = 
    forAll((xs: List[Int]) => sum(xs) == xs.sum)

  property("ex03 - length") = 
    forAll((xs: List[Int]) => length(xs) == xs.size)

  property("ex04 - map") = 
    forAll((xs: List[Int], f: Int => Boolean) => Exercises.map(xs, f) == xs.map(f))

  property("ex05 - filter") = 
    forAll((xs: List[Int], p: Int => Boolean) => filter(xs, p) == xs.filter(p))

  property("ex06 - append") = 
    forAll((xs: List[Int], ys: List[Int]) => append(xs, ys) == xs ++ ys)

  property("ex07 - concat") = 
    forAll((xs: List[List[Int]]) => concat(xs) == xs.flatten) 

  property("ex08 - concatMap") =
    forAll((xs: List[Int], f: Int => List[Boolean]) => concatMap(xs, f) == xs.flatMap(f))

  property("ex09 - maximum") = 
    forAll((xs: List[Int]) => xs.nonEmpty ==> (maximum(xs) == xs.max))

  property("ex10 - reverse") = 
    forAll((xs: List[Int]) => reverse(xs) == xs.reverse)
}
