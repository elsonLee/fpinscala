package chapter05

/**
  * Created by lexun on 7/5/2016.
  */
object Module {

  def ones: Stream[Int] = Stream.cons(1, ones)

  // exercise 5.8
  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  // exercise 5.9
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  // exercise 5.10
  def fibs: Stream[Int] = {
    def helper(pp: => Int, p: => Int): Stream[Int] = {
      lazy val next: Int = pp + p
      Stream.cons(pp, helper(p, next))
    }
    helper(0, 1)
  }

  // exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =  f(z) match {
      case Some((na, ns)) => Stream.cons(na, unfold(ns)(f))
      case None => Empty
  }

  // exercise 5.12
  def onesUnfold: Stream[Int] =
    unfold(1)(s => Some(s, s))

  def constantUnfold[A](a: A): Stream[A] =
    unfold(a)(s => Some(s, s))

  def fromUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some(s + 1, s + 1))

  def fibsUnfold: Stream[Int] =
    unfold((0, 1)){ case (pp, p) => {
      Some(pp, (p, pp + p))
    }}



}
