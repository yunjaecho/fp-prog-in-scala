package com.yunja.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    //case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](ds: List[A]): List[A] = ds match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }


  def setHead[A](ds: List[A], input: A): List[A] = ds match {
    case Nil => sys.error("setHead of empty list")
    case Cons(_, xs) => Cons(input, xs)
  }

  def drop[A](ds: List[A], n: Int): List[A] = ds match {
    case Nil => sys.error("drop of empty list")
    case Cons(_, xs) if n > 1 => drop(xs, n -1)
    case Cons(_, xs) if n == 1 => xs
  }

  def dropWhile[A](l: List[A]) (f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)


}

object ListType extends App {
  import List._

  val ex1: List[Double]= Nil
  val ex2: List[Int] = Cons(1, Nil)
  val ex3: List[String] = Cons("a", Cons("b", Nil))

  println("ex1 : " + ex1)
  println("ex2 : " + ex2)
  println("ex3 : " + ex3)

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x // no
    case Nil => 42 // no
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // yes 1 + 2 (3)
    case Cons(h, t) => h + sum(t)  // no (15)
    case _ => 101 //
  }

  println(s"x : $x")

  val arr = apply("a", "b", "c", "d")
  println(arr match {
    case Nil => Nil
    case Cons(a, _) => a
  })

  // drop
  val dr = drop(List(1,2,3,4,5), 3)
  println(s"dr : $dr")

  val dw = dropWhile(List(1,2,3,4,5))(x => x == 3)
  println(s"dw : $dw")

  val ap = append(List(1,2,3), List(4,5,6))
  println(s"ap : $ap")
}