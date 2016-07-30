package chapter02

import scala.annotation.tailrec

/**
  * Created by lexun on 7/2/2016.
  */
object Module {

  // exercise 2.1
  def fib(n: Int): Int = {
    @tailrec
    def fibHelper(i: Int, pp: Int, p: Int): Int = {
      if (i == n) pp + p
      else fibHelper(i + 1, p, pp + p)
    }
    if (n == 0) 0
    else if (n == 1) 1
    else fibHelper(2, 0, 1)
  }

  // exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int): Boolean = {
      if (i >= as.length - 1) true
      else if (ordered(as(i), as(i + 1)) == false) false
      else loop(i + 1)
    }
    loop(0)
  }

  // exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => ((b: B) => f(a, b))

  // exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

}
