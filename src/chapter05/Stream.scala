package chapter05

import scala.collection.immutable.List
import Module._

/**
  * Created by lexun on 7/5/2016.
  */
sealed trait Stream[+A] {

  // exercise 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(x, xs) => x() :: xs().toList
  }

  // exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, xs) =>
      if (n > 0) Stream.cons(x(), xs().take(n - 1))
      else Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, xs) =>
      if (n > 0) xs().drop(n - 1)
      else this
  }

  // exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(x, xs) =>
      if (p(x())) Stream.cons(x(), xs().takeWhile(p))
      else Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(x, xs) => f(x(), xs().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // exercise 5.5
  def takeWhileRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)

  // exercise 5.6
  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

  def append[B >: A](t: => Stream[B]): Stream[B] =
    foldRight(t)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => f(a) append b)

  // exercise 5.13
  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this){
      case Empty => None
      case Cons(x, xs) => Some(f(x()), xs())
    }

  def takeUnfold(n: Int): Stream[A] =
    unfold((this, n)){
      case (Empty, n) => None
      case (Cons(x, xs), n) =>
        if (n > 0)
          Some(x(), (xs(), n - 1))
        else
          None
    }

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Empty => None
      case Cons(x, xs) =>
        if (p(x()))
          Some(x(), xs())
        else
          None
    }


}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A] (hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}

