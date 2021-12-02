// Eryk Krupa
// Zadanie 1
import java.util.NoSuchElementException
import scala.annotation.tailrec

@tailrec
def last[A](xs: List[A]): A =
  if (xs == List())
      throw new NoSuchElementException("last of empty list")
  else if (xs == List(xs.head))
      xs.head
  else
    last(xs.tail)

last(List(1,9,5,6,3)) == 3
last(List("Ala", "ma", "kota")) == "kota"
last(List(5)) == 5
last(List(1, 2, 3, 4)) == 4
last(List(10, 2, 3, 4, 2)) == 2
last(List(3)) == 3
last(List())
// last(Nil) //=>> java.util.NoSuchElementException: last of empty list
