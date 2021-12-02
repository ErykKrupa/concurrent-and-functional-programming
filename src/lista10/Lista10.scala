// Eryk Krupa

package lista10

import java.util.concurrent.{ArrayBlockingQueue, Executors, Semaphore}
import scala.concurrent._
import scala.math.random
import scala.util.Random.nextBoolean

// zadanie 1
class Producer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name) {
  override def run(): Unit =
    for (i <- 1 to 10) {
      println(s"$getName producing $i"); buf.put(i)
    }
}

class Consumer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name) {
  override def run(): Unit =
    for (_ <- 1 to 10) println(s"$getName consumed ${buf.take}")
}

object Zad1a extends App {
  val buf: ArrayBlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
  new Producer("Producer", buf).start()
  new Consumer("Consumer", buf).start()
}

object Zad1b extends App {
  val buf: ArrayBlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
  new Producer("Producer1", buf).start()
  new Producer("Producer2", buf).start()
  new Consumer("Consumer1", buf).start()
  new Consumer("Consumer2", buf).start()
  new Consumer("Consumer3", buf).start()
  // Program się nie kończy, ponieważ producenci łącznie produkują 20 wartości,
  // podczas gdy konsumenci oczekują ich 30,
  // przez co wątki konsumentów wciąż oczekują, nie kończąc swojej pracy.
}

object Zad1c extends App {
  def produce(name: String, buf: ArrayBlockingQueue[Int]): Runnable =
    () => for (i <- 1 to 10) {
      println(s"$name producing $i"); buf.put(i)
    }

  def consume(name: String, buf: ArrayBlockingQueue[Int]): Runnable =
    () => for (_ <- 1 to 10) println(s"$name consumed ${buf.take}")

  val buf: ArrayBlockingQueue[Int] = new ArrayBlockingQueue[Int](5)
  val execCtx = ExecutionContext.global
  //  val execCtx = Executors.newCachedThreadPool()
  execCtx.execute(produce("Producer1", buf))
  execCtx.execute(produce("Producer2", buf))
  execCtx.execute(consume("Consumer1", buf))
  execCtx.execute(consume("Consumer2", buf))
  execCtx.execute(consume("Consumer3", buf))
  // Program kończy się natychmiastowo, ponieważ wątki tworzone przez
  // ExecutionContext.global są domyślnie daemon'ami.
  // By stworzyć wątki nie-daemon'y,
  // można wykorzystać Executors.newCachedThreadPool()
}

// zadanie 2
object Zad2 extends App {

  object PhilosopherStates extends Enumeration {
    type PhilosopherState = Value

    val Eating, LookingForChopsticks, Knocking, Meditating = Value
  }

  class Philosopher(private[this] val position: Int,
                    private[this] val leftStick: Semaphore,
                    private[this] val rightStick: Semaphore,
                    private[this] val environment: Environment
                   ) extends Thread {

    private[this] val name = s"Philosopher $position"

    var state: PhilosopherStates.Value = PhilosopherStates.Meditating
    var cycles = 0

    override def run(): Unit = {
      while (true) {
        state = PhilosopherStates.Knocking
        println(s"$name: hungry!")
        environment.diningRoom.acquire()

        state = PhilosopherStates.LookingForChopsticks
        println(s"$name: entering the dining room")
        if (nextBoolean()) {
          leftStick.acquire()
          rightStick.acquire()
        } else {
          rightStick.acquire()
          leftStick.acquire()
        }

        state = PhilosopherStates.Eating
        cycles += 1
        val eatingTime = ((random() + 0.5) * environment.meanEatingTime).toInt
        println(s"$name: om nom nom... (" + eatingTime / 1000.0 + s" s)")
        Thread.sleep(eatingTime)
        leftStick.release()
        rightStick.release()
        environment.diningRoom.release()

        state = PhilosopherStates.Meditating
        val meditatingTime = ((random() + 0.5) * environment.meanMeditatingTime).toInt
        println(s"$name: hmm... (" + meditatingTime / 1000.0 + " s)")
        Thread.sleep(meditatingTime)
      }
    }
  }

  class Environment(private val amountOfPhilosophers: Int,
                    val meanEatingTime: Int,
                    val meanMeditatingTime: Int,
                    private val doormanLogTime: Int) {
    private[this] val philosophers = new Array[Philosopher](amountOfPhilosophers)
    private[this] val chopsticks = new Array[Semaphore](amountOfPhilosophers)

    val diningRoom = new Semaphore(amountOfPhilosophers - 1)

    for (i <- 0 until amountOfPhilosophers)
      chopsticks(i) = new Semaphore(1)

    for (i <- 0 until amountOfPhilosophers)
      philosophers(i) = new Philosopher(i, chopsticks(i),
        chopsticks((i + 1) % amountOfPhilosophers), this)

    def start(): Unit = {
      for (philosopher <- philosophers)
        philosopher.start()
      while (true) {
        Thread.sleep(doormanLogTime)
        var sum = philosophers.count(it => it.state == PhilosopherStates.Eating)
        println(s"Doorman: $sum philosophers are eating")
        sum = philosophers.count(it => it.state == PhilosopherStates.LookingForChopsticks)
        println(s"Doorman: $sum philosophers are looking for chopsticks")
        sum = philosophers.count(it => it.state == PhilosopherStates.Knocking)
        println(s"Doorman: $sum philosopher is knocking to the dining room")
        sum = philosophers.count(it => it.state == PhilosopherStates.Meditating)
        println(s"Doorman: $sum philosophers are meditating")
        print(s"Doorman: eating times- ")
        philosophers.foreach(it => print(it.cycles + " "))
        println()
      }
    }
  }

  new Environment(10, 1000, 100, 5000).start()

  // Strategia polegająca na poczekaniu, aż obie pałeczki będą wolne,
  // może powodować zagłodzenie dwóch filozofów.
  // Dla przykładu, w przypadku, gdy jest czterech filozofów (A, B, C, D),
  // z których jedzą A i C, to gdy jeden z nich przestanie jeść,
  // filozofowie B i D dalej będą czekać na drugą pałeczke.
  // Nim C skończy jeść, może się okazać, że A zakończył medytację,
  // i z uwagi na to, że miał dwie wolne pałeczki, zaczął jeść.
  // Ten proces może trwać potencjalnie w nieskończoność,
  // głodząc filozofów B i D.

  // Jeśli co najmniej dwóm filozofom, siedzącym przy stole,
  // brakuje co najmniej jednego sąsiada,
  // to co najmniej jeden filozof może jeść,
  // ponieważ jeśli jest maksymalnie n - 1 filozofów i n pałeczek,
  // to przynajmniej jeden filozof musi mieć dostępne dwie pałeczki
  // (zasada szufladkowa Dirichleta).
}
