package chapter04

/**
  * Created by lexun on 7/3/2016.
  */
sealed trait Either[+E, +A] {
  // exercise 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(x), Right(y)) => Right(f(x, y))
    case (Left(x), Left(y)) => Left(x)          // how to deal with it ?
    case (Left(x), _) => Left(x)
    case (_, Left(x)) => Left(x)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
