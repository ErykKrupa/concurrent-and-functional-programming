// Eryk Krupa

package lista9

import java.util.concurrent.Semaphore

// zadanie 1
// W zadaniu 1 wartości licznika są różne w każdym wywołaniu,
// ponieważ program pracuje niedeterministycznie.
// Kiedy jeden wątek pobrał już licznik, ale nie zdążył go zinkrementować,
// mógł zostać wywłaszczony, przez co kolejny wątek pobrał tę samą wartość licznika.

object Zad1 extends App {
  var counter = 0

  def readWriteCounter(): Unit = counter += 1 // <-- tu jest problem
  val p = new Thread(() => for (_ <- 0 until 200000) readWriteCounter())
  val q = new Thread(() => for (_ <- 0 until 200000) readWriteCounter())
  val startTime = System.nanoTime
  p.start()
  q.start()
  p.join()
  q.join()
  val estimatedTime = (System.nanoTime - startTime) / 1000000
  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}

object Zad1Mon extends App {
  var counter = 0

  def readWriteCounter(): Unit = this.synchronized {
    counter += 1
  }

  val p = new Thread(() => for (_ <- 0 until 200000) readWriteCounter())
  val q = new Thread(() => for (_ <- 0 until 200000) readWriteCounter())
  val startTime = System.nanoTime
  p.start()
  q.start()
  p.join()
  q.join()
  val estimatedTime = (System.nanoTime - startTime) / 1000000
  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}

object Zad1Sem extends App {
  var counter = 0
  val semaphore = new Semaphore(1)

  def readWriteCounter(): Unit = {
    semaphore.acquire()
    counter += 1
    semaphore.release()
  }

  val p = new Thread(() => for (_ <- 0 until 200000) readWriteCounter())
  val q = new Thread(() => for (_ <- 0 until 200000) readWriteCounter())
  val startTime = System.nanoTime
  p.start()
  q.start()
  p.join()
  q.join()
  val estimatedTime = (System.nanoTime - startTime) / 1000000
  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}

// zadanie 2
object Zad2 extends App {
  def parallel[A, B](block1: => A, block2: => B): (A, B) = {
    //    val thread1 = new Thread() {
    //      var result: A = _
    //      override def run(): Unit = result = block1
    //    }
    //
    //    val thread2 = new Thread() {
    //      var result: B = _
    //      override def run(): Unit = result = block2
    //    }
    // Powyższe rozwiązanie nie działa, wyświetla błąd:
    // Parameter type in structural refinement may not refer to an abstract type defined outside that refinement
    // Wiem w czym tkwi problem, ale nie wiem jak to naprawić, żeby móc używać anonimowych klas w tym miejscu

    // Poniższe rozwiązanie działa z wykorzystaniem klasy wewnętrznej nieanonimowej,
    // ale nie wiem, czy jest to najlepsze rozwiązanie

    class Task[T](block: => T) extends Thread() {
      var result: T = _

      override def run(): Unit = result = block
    }

    val thread1 = new Task(block1)
    val thread2 = new Task(block2)
    thread1.start()
    thread2.start()
    thread1.join()
    thread2.join()
    (thread1.result, thread2.result)
  }

  println(parallel("a" + 1, "b" + 2))
  println(parallel("a" + 1, "b" + 2) == ("a1", "b2"))
  println(parallel(Thread.currentThread.getName, Thread.currentThread.getName))
  println(parallel({
    Thread.sleep(1000)
    "TEST1"
  }, {
    Thread.sleep(50)
    "TEST2"
  }) == ("TEST1", "TEST2"))
}

// zadanie 3
object Zad3 extends App {
  def periodically(duration: Long, times: Int)(block: => Unit): Unit = {
    val thread = new Thread(() =>
      for (_ <- 1 to times) {
        Thread.sleep(duration)
        block
      }
    )
    thread.setDaemon(true)
    thread.start()
  }

  periodically(1000, 5) {
    print("y ")
  }
  periodically(1000, 25) {
    print("x ")
  }
  Thread.sleep(10000)
  println("Done sleeping")

  // Zostały utworzone wątki daemony, czyli pomocnicze wątki, które giną,
  // gdy nie ma już innych wątków nie-daemonów.
  // Jedynym wątkiem nie-daemonem jest tu main, więc wątek wypisujący "x"
  // zginął, gdy main zakończył działanie.
}
