// Eryk Krupa
// zadanie 1
val suma: List[Double] => Double = xs =>
  if (xs == Nil)
    0.0
  else
    xs.head + suma(xs.tail)

suma(Nil) == 0.0
suma(List(-1, 2, 3)) == 4.0
suma(List(5.6)) == 5.6
suma(List()) == 0.0

// zadanie 2
def ends[A](xs: List[A]): (A, A) =
  if (xs == Nil)
    throw new NoSuchElementException("empty list")
  else if (xs.tail == Nil)
    (xs.head, xs.head)
  else
    (xs.head, ends(xs.tail) _2)

ends(List(1, 3, 5, 6, 9)) == (1, 9)
ends(List("Ala", "ma", "kota")) == ("Ala", "kota")
ends(List(1)) == (1, 1)
ends(List(-5.2, 1.0, 5.6)) == (-5.2, 5.6)
//ends(Nil) //=>> wyjątek NoSuchElementException: empty list

// zadanie 3
val posortowana: List[Int] => Boolean = xs =>
  if (xs == Nil || xs.tail == Nil)
    true
  else
    xs.head <= xs.tail.head && posortowana(xs.tail)

posortowana(List(1, 3, 3, 5, 6, 7))
posortowana(List(1, 1, 1, 1, 1, 1))
!posortowana(List(1, 1, 2, 1, 1, 1))
!posortowana(List(3, 2, 1))
posortowana(List(7))
posortowana(Nil)

// zadanie 4
val glue: (List[String], String) => String = (xs, s) =>
  if (xs == Nil)
    ""
  else if (xs.tail == Nil)
    xs.head
  else
    xs.head + s + glue(xs.tail, s)

glue(List("To", "jest", "napis"), "-") == "To-jest-napis"
glue(Nil, "-") == ""
glue(List("TEST"), "") == "TEST"
glue(List("TEST"), "niewidoczny separator") == "TEST"
glue(List("IN", "FOR", "MA", "TY", "KA"), "") == "INFORMATYKA"
