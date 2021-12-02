// Eryk Krupa

package lista8

import scala.reflect.ClassTag

// zadanie 1
class FullException(msg: String) extends Exception(msg)

abstract class MyQueue[E] {
  @throws[FullException]
  def enqueue(x: E): Unit

  def dequeue(): Unit

  @throws[NoSuchElementException]
  def first: E

  def isEmpty: Boolean

  def isFull: Boolean
}

class QueueMut[E: ClassTag] private(val capacity: Int = 1000) extends MyQueue[E] {
  private[this] val workingCapacity = capacity + 1
  private[this] val elems = new Array[E](workingCapacity)
  private[this] var in, out = 0

  override def enqueue(x: E): Unit = {
    if (isFull)
      throw new FullException("Full capacity of queue used")
    else {
      elems(in) = x
      in = (in + 1) % workingCapacity
    }
  }

  override def dequeue(): Unit = {
    if (!isEmpty) {
      elems(out) = null.asInstanceOf[E]
      out = (out + 1) % workingCapacity
    }
  }

  override def first: E =
    if (isEmpty) throw new NoSuchElementException
    else elems(out)

  override def isEmpty: Boolean = in == out

  override def isFull: Boolean = out == (in + 1) % workingCapacity
}

object QueueMut {
  def apply[E: ClassTag](xs: E*): QueueMut[E] = {
    val queue = empty[E]()
    for (x <- xs) queue.enqueue(x)
    queue
  }

  def empty[E: ClassTag](capacity: Int = 1000): QueueMut[E] = new QueueMut[E](capacity)
}

object Lista8 {
  def main(args: Array[String]): Unit = {
    val queue: QueueMut[Int] = QueueMut.empty(3)
    println(queue.isEmpty)
    println(!queue.isFull)
    queue.enqueue(0)
    println(!queue.isFull)
    println(!queue.isEmpty)
    println(queue.first == 0)
    println(!queue.isEmpty)
    queue.dequeue()
    println(queue.isEmpty)
    try {
      queue.first
    } catch {
      case _: NoSuchElementException => println(true)
      case _ => println(false)
    }
    queue.enqueue(1)
    queue.enqueue(2)
    queue.enqueue(3)
    println(queue.isFull)
    try {
      queue.enqueue(5)
    } catch {
      case _: FullException => println(true)
      case _ => println(false)
    }
    queue.dequeue()
    println(!queue.isFull)
    println(queue.first == 2) // pierwszy element w kolejce to 2
    queue.enqueue(4) // dodanie elementu na początku fizycznej listy
    println(queue.first == 2) // dowód, że pierwszy element w kolejce się nie zmienił
    println(queue.isFull)
    queue.dequeue()
    queue.dequeue()
    println(!queue.isEmpty)
    queue.dequeue()
    println(queue.isEmpty)
    queue.enqueue(5)
    println(queue.first == 5)
    println(!queue.isEmpty)
    queue.dequeue()
    println(queue.isEmpty)

    val queueString: QueueMut[String] = QueueMut.empty(3)
    queueString.enqueue("ABC")
    println(!queueString.isEmpty)
    queueString.dequeue()
    println(queueString.isEmpty)
  }
}
