// Eryk Krupa
import scala.annotation.tailrec

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

val t6 = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
val t3 = Node(1, Node(2, Empty, Empty), Empty)
val t1 = Node(1, Empty, Empty)
val t0 = Empty
val t12 = Node(1, Node(2, Empty, Node(3, t6, Empty)), Empty)
val t30 = Node(1, Node(2, t6, Node(3, t6, t6)), t6)

// zadanie 1
val sumBT: BT[Int] => Int = {
  case Node(e, l, r) => e + sumBT(l) + sumBT(r)
  case Empty => 0
}

sumBT(t6) == 6
sumBT(t3) == 3
sumBT(t1) == 1
sumBT(t0) == 0
sumBT(t12) == 12
sumBT(t30) == 30

// zadanie 2
def foldBT[A, B](f: A => (B, B) => B)(acc: B)(bt: BT[A]): B = bt match {
  case Node(e, l, r) => f(e)(foldBT(f)(acc)(l), foldBT(f)(acc)(r))
  case Empty => acc
}

// zadanie 3
val sumBTfold: BT[Int] => Int
= bt => foldBT[Int, Int](x => (y1, y2) => x + y1 + y2)(0)(bt)

sumBTfold(t6) == 6
sumBTfold(t3) == 3
sumBTfold(t1) == 1
sumBTfold(t0) == 0
sumBTfold(t12) == 12
sumBTfold(t30) == 30

def inorderBTfold[A](bt: BT[A]): List[A] = foldBT[A, List[A]](x => (y1, y2) => y1 ::: x :: y2)(Nil)(bt)

def preorderBTfold[A](bt: BT[A]): List[A] = foldBT[A, List[A]](x => (y1, y2) => x :: y1 ::: y2)(Nil)(bt)

def postorderBTfold[A](bt: BT[A]): List[A] = foldBT[A, List[A]](x => (y1, y2) => y1 ::: y2 ::: x :: Nil)(Nil)(bt)

inorderBTfold(t6) == List(2, 3, 1)
preorderBTfold(t6) == List(1, 2, 3)
postorderBTfold(t6) == List(3, 2, 1)
val tree = Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(6, Empty, Empty), Node(7, Empty, Empty)))
//      1
//  2       3
//4   5   6   7

inorderBTfold(tree) == List(4, 2, 5, 1, 6, 3, 7)
preorderBTfold(tree) == List(1, 2, 4, 5, 3, 6, 7)
postorderBTfold(tree) == List(4, 5, 2, 6, 7, 3, 1)
inorderBTfold(t1) == List(1)
preorderBTfold(t1) == List(1)
postorderBTfold(t1) == List(1)
inorderBTfold(t0) == Nil
preorderBTfold(t0) == Nil
postorderBTfold(t0) == Nil

// zadanie 4
def mapBT[A, B](f: A => B)(bt: BT[A]): BT[B] =
  foldBT[A, BT[B]](x => (y1, y2) => Node(f(x), y1, y2))(Empty)(bt)

mapBT[Int, Int](v => v * 2)(t6) == Node(2, Node(4, Empty, Node(6, Empty, Empty)), Empty)
mapBT[Int, Int](v => v * 3)(t0) == Empty
mapBT[Int, Int](v => v - 1)(t3) == Node(0, Node(1, Empty, Empty), Empty)

// zadanie 5
sealed trait Graphs[A]
case class Graph[A](succ: A => List[A]) extends Graphs[A]

def pathExists[A](g: Graph[A])(from: A, to: A): Boolean = {
  @tailrec
  def searchPath(visited: List[A])(queue: List[A]): Boolean =
    queue match {
      case h :: t =>
        if (visited contains h) searchPath(visited)(t)
        else if (h == to) true
        else searchPath(h :: visited)(t ::: (g succ h))
      case Nil => false
    }

  searchPath(Nil)(g succ from)
}

val g = Graph((i: Int) =>
  i match {
    case 0 => List(3)
    case 1 => List(0, 2, 4)
    case 2 => List(1)
    case 3 => List(5)
    case 4 => List(0, 2)
    case 5 => List(3)
    case n => throw new NoSuchElementException(s"Graph g: node $n doesn't exist")
  })
pathExists(g)(4, 1)
!pathExists(g)(0, 4)
!pathExists(g)(3, 0)
pathExists(g)(2,2)
!pathExists(g)(0,0)

val g1 = Graph((i: String) =>
  i match {
    case "A" => List("B")
    case "B" => List("C")
    case "C" => List("D")
    case "D" => List("A")
    case n => throw new NoSuchElementException(s"Graph g1: node $n doesn't exist")
  })
pathExists(g1)("A", "D")
pathExists(g1)("C", "D")
pathExists(g1)("D", "B")
pathExists(g1)("C","C")
pathExists(g1)("B","B")

val g1 = Graph((i: Double) =>
  i match {
    case 0.0 => List(0.0)
    case 0.5 => List(0.5)
    case 1.0 => List(1.0)
    case n => throw new NoSuchElementException(s"Graph g2: node $n doesn't exist")
  })
pathExists(g1)(0.0, 0.0)
!pathExists(g1)(0.5, 1)
!pathExists(g1)(1, 0.5)
!pathExists(g1)(1, 0.7)
pathExists(g1)(0.7, 1)  // throw new NoSuchElementException(s"Graph g2: node 0.7 doesn't exist")
