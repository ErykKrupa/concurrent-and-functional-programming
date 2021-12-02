// Eryk Krupa

package lista7

// zadanie 1
class UnderflowException(msg: String) extends Exception(msg)

class MyQueue[+T] private(private val firstList: List[T],
                          private val secondList: List[T]) {
  def this() = this(Nil, Nil)

  def enqueue[S >: T](elem: S): MyQueue[S] = firstList match {
    case Nil => new MyQueue[S](List(elem), Nil)
    case _ => new MyQueue[S](firstList, elem :: secondList)
  }

  def first: T = firstList match {
    case h :: _ => h
    case Nil => throw new UnderflowException("Empty list")
  }

  def firstOption: Option[T] = firstList match {
    case h :: _ => Option(h)
    case Nil => Option.empty
  }

  def dequeue: MyQueue[T] = firstList match {
    case Nil => MyQueue.empty
    case _ :: Nil => new MyQueue(secondList.reverse, Nil)
    case _ :: xs => new MyQueue(xs, secondList)
  }

  def isEmpty: Boolean = firstList == Nil
}

object MyQueue {
  def apply[T](list: T*): MyQueue[T] = new MyQueue[T](list.toList, Nil)

  def empty[T]: MyQueue[T] = new MyQueue[T](Nil, Nil)
}

// zadanie 2
sealed trait BT[+A]

case object Empty extends BT[Nothing]

case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

object Lista7 {
  def breadthBT[A](tree: BT[A]): List[A] = {
    def search(queue: MyQueue[BT[A]]): List[A] = {
      queue.firstOption match {
        case None => Nil
        case Some(Node(e, l, r)) => e :: search(queue.dequeue) ::: search(MyQueue(l)) ::: search(MyQueue(r))
        case Some(Empty) => search(queue.dequeue)
      }
    }

    search(MyQueue(tree))
  }

  val t: Node[Int] = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
  val tt: Node[Int] = Node(1, Node(2, Node(4, Empty, Empty), Empty),
    Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty))

  def main(args: Array[String]): Unit = {
    println((new MyQueue).firstOption.isEmpty)
    println(MyQueue().firstOption.isEmpty)
    println(MyQueue.empty.firstOption.isEmpty)
    println(MyQueue(1, 2, 3).firstOption.isDefined)

    val queue = MyQueue(1, 2, 3)
    val emptyQueue = MyQueue.empty

    println(!emptyQueue.enqueue(4).isEmpty)
    println(!queue.enqueue(4).isEmpty)
    println(emptyQueue.isEmpty)
    println(!queue.isEmpty)
    println(queue.enqueue(4).enqueue(5).dequeue.first == queue.enqueue(5).dequeue.enqueue(4).first)
    println(emptyQueue.enqueue(4).dequeue.isEmpty)
    println(emptyQueue.dequeue.isEmpty)
    println(emptyQueue.enqueue(4).enqueue(5).first == emptyQueue.enqueue(4).first)
    println(queue.enqueue(4).enqueue(5).first == queue.enqueue(5).first)
    println(emptyQueue.enqueue(4).first == 4)
    println(queue.enqueue(4).first == 1)
    try {
      emptyQueue.first
    } catch {
      case _: UnderflowException => println("true")
      case _ => println("false")
    }
    println(emptyQueue.enqueue(4).enqueue(5).firstOption == emptyQueue.enqueue(4).firstOption)
    println(queue.enqueue(4).enqueue(5).firstOption == queue.enqueue(5).firstOption)
    println(emptyQueue.enqueue(4).firstOption.contains(4))
    println(emptyQueue.firstOption.isEmpty)
    println(queue.dequeue.firstOption.isDefined)

    println(breadthBT(t) == List(1, 2, 3))
    println(breadthBT(tt) == List(1, 2, 4, 3, 5, 6))
    println(breadthBT(Empty) == Nil)
  }
}
