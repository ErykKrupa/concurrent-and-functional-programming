import scala.annotation.tailrec

// Eryk Krupa
// zadanie 1
@tailrec
def whileLoop(condition: => Boolean)(expression: => Unit): Unit = {
  if (condition) {
    expression
    whileLoop(condition)(expression)
  } else ()
}
// Pierwszy argument musi być wyrażeniem typu Boolean,
// ponieważ jest to warunek logiczny dla pętli while,
// który jest sprawdzany w każdej iteracji

// Drugi argument musi być wyrażeniem typu Unit,
// ponieważ chcemy przekazać ciało pętli while,
// które zwraca Unit, ponieważ celem pętli while nie jest zwracanie konkretnych wartości,
// a wywoływanie jakiś skutków ubocznych.

// Funkcja musi zwracać typ Unit z powodów opisanych wyżej

var count = 0

whileLoop(count < 5) {
  println(count)
  count += 1
}

// zadanie 2
def lrepeat[A](k: Int)(xsl: LazyList[A]): LazyList[A] = {
  def repeatOne(k: Int)(e: A): LazyList[A] =
    if (k > 0) e #:: repeatOne(k - 1)(e)
    else LazyList()

  xsl match {
    case h #:: t => repeatOne(k)(h) #::: lrepeat(k)(t)
    case LazyList() => LazyList()
  }
}

lrepeat(2)(LazyList(1, 2, 3)).force == LazyList(1, 1, 2, 2, 3, 3)
lrepeat(3)(LazyList('a', 'b', 'c', 'd')).force == LazyList('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd')
(lrepeat(3)(LazyList.from(1)) take 12).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
lrepeat(0)(LazyList("Ala", "ma", "kota")).force == LazyList()
lrepeat(-1)(LazyList("Ala", "ma", "kota")).force == LazyList()
lrepeat(10)(LazyList()).force == LazyList()
(lrepeat(3)(LazyList.from(1)) take 10).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4)
(lrepeat(2)(LazyList.from(1) take 3) take 7).toList == List(1, 1, 2, 2, 3, 3)

// zadanie 3
sealed trait lBT[+A]

case object LEmpty extends lBT[Nothing]

case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

def lBreadth[A](ltree: lBT[A]): LazyList[A] = {
  def search(queue: List[lBT[A]]): LazyList[A] = {
    queue match {
      case h :: t =>
        h match {
          case LNode(e, l, r) => e #:: search(t ::: l() :: r() :: Nil)
          case LEmpty => search(t)
        }
      case Nil => LazyList()
    }
  }

  search(List(ltree))
}


val tree1 = LNode(1, () => LNode(2, () => LNode(4, () => LEmpty, () => LEmpty), () => LNode(5, () => LEmpty, () => LEmpty)), () => LNode(3, () => LNode(6, () => LEmpty, () => LEmpty), () => LNode(7, () => LEmpty, () => LEmpty)))
//      1
//  2       3
//4   5   6   7

val tree2 = LNode("A", () => LNode("B", () => LEmpty, () => LNode("D", () => LEmpty, () => LEmpty)), () => LNode("C", () => LNode("E", () => LEmpty, () => LEmpty), () => LEmpty))
//      A
//  B       C
//    D   E

lBreadth(tree1) == (LazyList.from(1) take 7)
(lBreadth(tree1) take 5) == LazyList(1, 2, 3, 4, 5)
(lBreadth(tree1) take 0).toList == Nil
lBreadth(tree2) == LazyList("A", "B", "C", "D", "E")
(lBreadth(tree2) take 4) == LazyList("A", "B", "C", "D")
lBreadth(LEmpty) == LazyList()

def lTree(n: Int): lBT[Int] = {
  LNode(n, () => lTree(2 * n), () => lTree(2 * n + 1))
}

(lBreadth(lTree(1)) take 7) == (LazyList.from(1) take 7)
(lBreadth(lTree(-10)) take 4).toList == List(-10, -20, -19, -40)
(lBreadth(lTree(1)) take 0) == LazyList()
