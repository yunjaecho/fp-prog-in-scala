package com.yunja.chapter2

import scala.annotation.tailrec

object HigherOrderFunction extends App {

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) return acc
      else go(n-1, n * acc)
    }

    go(n, 1)
  }

  def fib1( n : Int) : Int = n match {
    case 0 | 1 => n
    case _ => fib1( n-1 ) + fib1( n-2 )
  }

  def formatResult(name: String, n: Int, f:Int => Int) = {
    val msg = "The %s of %d is %d"
    println(msg.format(name, n, f(n)))
  }

  def findFirst(ss: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n > ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }

    loop(0)
  }

  def findFirst2[A](as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n > as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n > as.length - 1) true
      else if (n > 0 && ordered(as(n - 1), as(n))) false
      else loop(n + 1)
    }

    loop(0)
  }


  formatResult("absolute value", -42, abs)
  formatResult("factorial", 7, factorial)

  val findN = findFirst(Array("a", "b", "c"), "b")
  println(s"findN : $findN")

  val findN2 = findFirst2(Array("a", "b", "c"), (x: String) => x == "b")
  println(s"findN2 : $findN2")

  val stored = isSorted(Array(1,2,3,4), (x: Int, y: Int) => x > y)
  println(s"stored : $stored")

}
