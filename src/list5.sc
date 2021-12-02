// Eryk Krupa
// zadanie 1
class MyPair[A, B](var fst: A, var snd: B) {
  override def toString: String = s"($fst, $snd)"
}

val pair = new MyPair[Int, String](4, "ABC")
pair.fst == 4
pair.snd == "ABC"
pair.toString == "(4, ABC)"
pair.fst = 2
pair.snd = "QWERTY"
pair.fst == 2
pair.snd == "QWERTY"
pair.toString == "(2, QWERTY)"

// zadanie 2
class BankAccount(initialBalance: Double) {
  private[this] var balance = initialBalance

  def checkBalance: Double = balance

  def deposit(amount: Double): Double = {
    balance += amount
    balance
  }

  def withdraw(amount: Double): Double = {
    balance -= amount
    balance
  }

  override def toString = "%.2f".format(balance)
}

class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  override def deposit(amount: Double) = super.deposit(amount - 1)

  override def withdraw(amount: Double) = super.withdraw(amount + 1)
}

val checkingAccount = new CheckingAccount(1000)
checkingAccount.checkBalance == 1000
checkingAccount.deposit(500)
checkingAccount.checkBalance == 1499
checkingAccount.withdraw(100)
checkingAccount.checkBalance == 1398

class SavingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  private[this] var transactionsInMonth = 0

  def earnMonthlyInterest(): Unit = {
    super.deposit(checkBalance * 0.01)
    transactionsInMonth = 0
  }

  override def deposit(amount: Double): Double = {
    transactionsInMonth += 1
    super.deposit(amount - (if (transactionsInMonth < 4) 0 else 1))
  }

  override def withdraw(amount: Double): Double = {
    transactionsInMonth += 1
    super.withdraw(amount + (if (transactionsInMonth < 4) 0 else 1))
  }
}

val savingAccount = new SavingAccount(1000)
savingAccount.deposit(500)
savingAccount.checkBalance == 1500
savingAccount.withdraw(500)
savingAccount.checkBalance == 1000
savingAccount.deposit(500)
savingAccount.checkBalance == 1500
savingAccount.withdraw(499)
savingAccount.checkBalance == 1000
savingAccount.withdraw(-501)
savingAccount.checkBalance == 1500
savingAccount.earnMonthlyInterest()
savingAccount.checkBalance == 1515
savingAccount.withdraw(15)
savingAccount.checkBalance == 1500
savingAccount.withdraw(500)
savingAccount.checkBalance == 1000
savingAccount.deposit(500)
savingAccount.checkBalance == 1500
savingAccount.deposit(-499)
savingAccount.checkBalance == 1000
savingAccount.deposit(501)
savingAccount.checkBalance == 1500

val savingAccountWithDebit = new SavingAccount(-1000)
savingAccountWithDebit.deposit(500)
savingAccountWithDebit.checkBalance == -500
savingAccountWithDebit.withdraw(500)
savingAccountWithDebit.checkBalance == -1000
savingAccountWithDebit.deposit(500)
savingAccountWithDebit.checkBalance == -500
savingAccountWithDebit.deposit(-499)
savingAccountWithDebit.checkBalance == -1000
savingAccountWithDebit.deposit(501)
savingAccountWithDebit.checkBalance == -500
savingAccountWithDebit.earnMonthlyInterest()
savingAccountWithDebit.checkBalance == -505
savingAccountWithDebit.deposit(5)
savingAccountWithDebit.checkBalance == -500
savingAccountWithDebit.withdraw(500)
savingAccountWithDebit.checkBalance == -1000
savingAccountWithDebit.deposit(500)
savingAccountWithDebit.checkBalance == -500
savingAccountWithDebit.withdraw(499)
savingAccountWithDebit.checkBalance == -1000
savingAccountWithDebit.withdraw(-501)
savingAccountWithDebit.checkBalance == -500

// zadanie 3
abstract class Zwierz(val imie: String = "bez imienia") {

  def rodzaj: String = getClass.getSimpleName

  def dajGlos: String

  override def toString = s"$rodzaj $imie daje glos $dajGlos!"
}

class Pies(override val imie: String = "bez imienia") extends Zwierz {
  def dajGlos: String = "Hau Hau"
}

class Kot(override val imie: String = "bez imienia") extends Zwierz {
  def dajGlos: String = "Miau Miau"
}

object TestZwierza {
  def main(args: Array[String]): Unit = {
    val zwierzeta: Vector[Zwierz] = Vector(
      new Pies(),
      new Kot(),
      new Pies("Andrzej"),
      new Kot("Błażej")
    )
    for (zwierz <- zwierzeta) {
      println(s"Rodzaj: ${zwierz.rodzaj}")
      println(s"Imie: ${zwierz.imie}")
      println(s"Glos: ${zwierz.dajGlos}")
      println(s"${zwierz.toString}")
      println()
    }
  }
}

TestZwierza.main(Array())
