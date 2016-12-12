package com.impatient.ch8

class BankAccount(initialBalance: Double) {
  private var balance = initialBalance

  def currentBalance = balance

  def deposit(amount: Double) = {
    balance += amount
    balance
  }

  def withdraw(amount: Double) = {
    balance -= amount
    balance
  }
}

class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  private var balance = initialBalance

  override def deposit(amount: Double) = {
    balance += amount - 1
    balance
  }

  override def withdraw(amount: Double) = {
    balance -= (amount + 1)
    balance
  }
}

class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  private var balance = initialBalance
  private var freeTransactions = 3
  private val interestFactor = 0.01

  def earnMonthlyInterest():Double = {
    balance *= interestFactor
    freeTransactions = 3
    balance
  }

  override def deposit(amount: Double) = {
    if(freeTransactions == 0)  {
      balance += amount
      balance -= 1
    }
    else {
      balance += amount
      freeTransactions -= 1
    }
    balance
  }

  override def withdraw(amount: Double) = {
    if(freeTransactions == 0)  {
      balance -= amount
      balance -= 1
    }
    else {
      balance -= amount
      freeTransactions -= 1
    }
    balance
  }
}

object accountUser {
  def main(args: Array[String]): Unit = {
    val savingsAccount = new SavingsAccount(1000)
    println(savingsAccount.withdraw(100))
    println(savingsAccount.withdraw(100))
    println(savingsAccount.withdraw(100))
    println(savingsAccount.withdraw(100))
  }
}

