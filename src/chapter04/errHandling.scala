package chapter04

/**
  * Created by lexun on 6/29/2016.
  */
object errHandling {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  // exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case (_, _) => None
    }

  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x, y)))

  // exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((a, b) => map2(a, b)((x, xs) => x :: xs))

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a.map(i => Try{i.toInt}))

  // exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((a, b) => map2(f(a), b)((x, xs) => x :: xs))

  def parseIntsFast(a: List[String]): Option[List[Int]] =
    traverse(a)(i => Try(i.toInt))

  // exercise 4.7
  def sequenceEither[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(Nil): Either[E, List[A]])((a, b) => a.map2(b)((x, xs) => x :: xs))

  def traverseEither[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]])((a, b) => f(a).map2(b)((x, xs) => x :: xs))

  def TryEither[A](a: => A): Either[String, A] =
    try Right(a)
    catch { case e: Exception => Left("catch you") }

  def parseIntsFastEither(a: List[String]): Either[String, List[Int]] =
    traverseEither(a)(i => TryEither(i.toInt))

}
