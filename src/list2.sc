// Eryk Krupa

import scala.annotation.tailrec
import scala.math.{abs, cbrt}

// zadanie 1
def take[A](n: Int, xs: List[A]): List[A] =
  xs match {
    case head :: tail => if (n > 0) head :: take(n - 1, tail) else Nil
    case Nil => Nil
  }

take(2, List(1, 2, 3, 5, 6)) == List(1, 2)
take(-2, List(1, 2, 3, 5, 6)) == Nil
take(8, List(1, 2, 3, 5, 6)) == List(1, 2, 3, 5, 6)
take(0, List(1, 2, 3, 5, 6)) == Nil
take(2, Nil) == Nil

// zadanie 2
@tailrec
def drop[A](n: Int, xs: List[A]): List[A] =
  xs match {
    case _ :: tail => if (n > 0) drop(n - 1, tail) else xs
    case Nil => xs
  }

drop(2, List(1, 2, 3, 5, 6)) == List(3, 5, 6)
drop(-2, List(1, 2, 3, 5, 6)) == List(1, 2, 3, 5, 6)
drop(8, List(1, 2, 3, 5, 6)) == Nil
drop(0, List(1, 2, 3, 5, 6)) == List(1, 2, 3, 5, 6)
drop(2, Nil) == Nil

// zadanie 3
def reverse[A](xs: List[A]): List[A] = {
  @tailrec
  def reverseHelper(xs: List[A], acc: List[A]): List[A] =
    xs match {
      case head :: tail => reverseHelper(tail, head :: acc)
      case Nil => acc
    }

  reverseHelper(xs, Nil)
}

reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")
reverse(List(1, 2, 3, 5, 6)) == List(6, 5, 3, 2, 1)
reverse(List(4.3)) == List(4.3)
reverse(Nil) == Nil

// zadanie 4
val replicate: List[Int] => List[Int] = xs => {
  lazy val replicateHead: (Int, Int) => List[Int] =
    (repeat, head) =>
      if (repeat > 0)
        head :: replicateHead(repeat - 1, head)
      else
        Nil
  xs match {
    case head :: tail => replicateHead(head, head) ::: replicate(tail)
    case Nil => Nil
  }
}

replicate(List(1, 0, 4, -2, 3)) == List(1, 4, 4, 4, 4, 3, 3, 3)
replicate(List(1, 2, 3, 5, 6)) == List(1, 2, 2, 3, 3, 3, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6)
replicate(List(0)) == Nil
replicate(List(-5, -4, -3, -2, -1)) == Nil
replicate(Nil) == Nil

// zadanie 5
@tailrec
val root3: Double => Double = a => {
  val eps = 10e-15
  @tailrec
  def root3Helper(x: Double): Double =
    if (abs(x * x * x - a) <= eps * abs(a))
      x
    else
      root3Helper(x + (a / (x * x) - x) / 3)

  root3Helper(if (a > 1) a / 3 else a)
}

val eps = 10e-15
abs(root3(10) - cbrt(10)) < eps
abs(root3(1) - cbrt(1)) < eps
abs(root3(-6) - cbrt(-6)) < eps
abs(root3(0) - cbrt(0)) < eps
abs(root3(99999) - cbrt(99999)) < eps
abs(root3(0.0001) - cbrt(0.0001)) < eps
abs(root3(-0.5) - cbrt(-0.5)) < eps
