// Eryk Krupa

import scala.annotation.tailrec

// zadanie 1
@tailrec
def existsA[A](xs: List[A])(p: A => Boolean): Boolean =
  xs match {
    case h :: t => if (p(h)) true else existsA(t)(p)
    case Nil => false
  }

existsA(List(5, 1, 2, 3))(_ == 2)
!existsA(List(5, 1, 2, 3))(_ == 4)
!existsA(List(5, 1, 2, 3))(_ > 6)
existsA(List(5, 1, 2, 3))(_ >= 4)
existsA(List(5, 1, 2, 3))(_ => true)
!existsA(Nil: List[Int])(_ == 4)
!existsA(Nil: List[Int])(_ => true)
!existsA(List("qwerty", "test", ""))(_.length > 7)
existsA(List("qwerty", "test", ""))(_.length == 4)

def existsB[A](xs: List[A])(p: A => Boolean): Boolean =
  (xs foldLeft false) ((acc, x) => acc || p(x))

existsB(List(5, 1, 2, 3))(_ == 2)
!existsB(List(5, 1, 2, 3))(_ == 4)
!existsB(List(5, 1, 2, 3))(_ > 6)
existsB(List(5, 1, 2, 3))(_ >= 4)
existsB(List(5, 1, 2, 3))(_ => true)
!existsB(Nil: List[Int])(_ == 4)
!existsB(Nil: List[Int])(_ => true)
!existsB(List("qwerty", "test", ""))(_.length > 7)
existsB(List("qwerty", "test", ""))(_.length == 4)

def existsC[A](xs: List[A])(p: A => Boolean): Boolean =
  (xs foldRight false) ((x, acc) => acc || p(x))

existsC(List(5, 1, 2, 3))(_ == 2)
!existsC(List(5, 1, 2, 3))(_ == 4)
!existsC(List(5, 1, 2, 3))(_ > 6)
existsC(List(5, 1, 2, 3))(_ >= 4)
existsC(List(5, 1, 2, 3))(_ => true)
!existsC(Nil: List[Int])(_ == 4)
!existsC(Nil: List[Int])(_ => true)
!existsC(List("qwerty", "test", ""))(_.length > 7)
existsC(List("qwerty", "test", ""))(_.length == 4)

// zadanie 2
def filter[A](xs: List[A])(p: A => Boolean): List[A] =
  (xs foldRight (Nil: List[A])) ((x, acc) => if (p(x)) x :: acc else acc)

filter(List(2, 7, 1, 3, 7, 8, 4, 1, 6, 9))(_ > 3) == List(7, 7, 8, 4, 6, 9)
filter(List(2, 7, 1, 3, 7, 8, 4, 1, 6, 9))(_ > -1) == List(2, 7, 1, 3, 7, 8, 4, 1, 6, 9)
filter(List(2, 7, 1, 3, 7, 8, 4, 1, 6, 9))(_ > 10) == Nil
filter(List(2, 7, 1, 3, 7, 8, 4, 1, 6, 9))(_ => true) == List(2, 7, 1, 3, 7, 8, 4, 1, 6, 9)
filter(List(2, 7, 1, 3, 7, 8, 4, 1, 6, 9))(_ => false) == Nil
filter(Nil: List[Int])(_ > 0) == Nil
filter(Nil: List[Int])(_ => true) == Nil
filter(List("qwerty", "test", ""))(_.length > 5) == List("qwerty")

// zadanie 3
def remove1A[A](xs: List[A])(p: A => Boolean): List[A] =
  xs match {
    case h :: t => if (p(h)) t else h :: remove1A(t)(p)
    case Nil => Nil
  }

remove1A(List(1, 2, 3, 2, 5))(_ == 2) == List(1, 3, 2, 5)
remove1A(List(1, 2, 3, 2, 5))(_ == 6) == List(1, 2, 3, 2, 5)
remove1A(List(1, 1, 1, 1))(_ == 1) == List(1, 1, 1)
remove1A(List(1, 2, 3, 2, 5))(_ > 2) == List(1, 2, 2, 5)
remove1A(List(1, 2, 3, 2, 5))(_ => true) == List(2, 3, 2, 5)
remove1A(List(1, 2, 3, 2, 5))(_ => false) == List(1, 2, 3, 2, 5)
remove1A(Nil: List[Int])(_ => true) == Nil
remove1A(List("qwerty", "test", ""))(x => x.length < 5) == List("qwerty", "")

def remove1B[A](xs: List[A])(p: A => Boolean): List[A] = {
  @tailrec
  def removeFirst(xs: List[A], acc: List[A]): List[A] = {
    xs match {
      case h :: t => if (p(h)) acc reverse_::: t  else removeFirst(t, h :: acc)
      case Nil => acc reverse_::: Nil
    }
  }

  removeFirst(xs, Nil)
}

remove1B(List(1, 2, 3, 2, 5))(_ == 2) == List(1, 3, 2, 5)
remove1B(List(1, 2, 3, 2, 5))(_ == 6) == List(1, 2, 3, 2, 5)
remove1B(List(1, 1, 1, 1))(_ == 1) == List(1, 1, 1)
remove1B(List(1, 2, 3, 2, 5))(_ > 2) == List(1, 2, 2, 5)
remove1B(List(1, 2, 3, 2, 5))(_ => true) == List(2, 3, 2, 5)
remove1B(List(1, 2, 3, 2, 5))(_ => false) == List(1, 2, 3, 2, 5)
remove1B(Nil: List[Int])(_ => true) == Nil
remove1B(List("qwerty", "test", ""))(x => x.length < 5) == List("qwerty", "")

// zadanie 4
def splitAt[A](xs: List[A])(n: Int): (List[A], List[A]) = {
  @tailrec
  def splitAtWithIterAndAcc(xs: List[A], i: Int, acc: List[A]): (List[A], List[A]) = {
    xs match {
      case h :: t => if (i == n) (acc.reverse, h :: t)
      else splitAtWithIterAndAcc(t, i + 1, h :: acc)
      case Nil => (acc.reverse, Nil)
    }
  }
  if (n < 0) (Nil, xs) else splitAtWithIterAndAcc(xs, 0, Nil)
}

def test4[A](xs: List[A])(n: Int): Boolean =
  splitAt(xs)(n) == (xs take n, xs drop n)

splitAt(List('a', 'b', 'c', 'd', 'e'))(2) == (List('a', 'b'), List('c', 'd', 'e'))
test4(List('a', 'b', 'c', 'd', 'e'))(2)
test4(List('a', 'b', 'c', 'd', 'e'))(5)
test4(List('a', 'b', 'c', 'd', 'e'))(0)
test4(List('a', 'b', 'c', 'd', 'e'))(99)
test4(List('a', 'b', 'c', 'd', 'e'))(-3)
test4(Nil)(10)
test4(Nil)(-1000)
