package chapter10

import scala.collection.immutable.Stream.Empty

/**
  * Created by lexun on 7/18/2016.
  */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  // exercise 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero = true
  }

  // exercise 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)

    def zero = None
  }

  // exercise 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    def op(a1: (A) => A, a2: (A) => A): (A) => A = x => a1(a2(x))

    // a1 compose a2 is better
    def zero: (A) => A = x => x
  }

  // exercise 10.4
  // TODO

  // exercise 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // exercise 10.6
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)

    def zero: A = m.zero
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap[A, (B) => B](as, endoMonoid[B])(a => b => f(a, b))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap[A, (B) => B](as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  // exerice 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val len = v.length
    if (len == 0) m.zero
    else if (len == 1) f(v(0))
    else {
      val (l, r) = v.splitAt(len / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  // exercise 10.
  // TODO

  // exercise 10.9
  def isSorted(v: IndexedSeq[Int]): Boolean = {
    val sortMonoid: Monoid[(Int, Int, Boolean)] = new Monoid[(Int, Int, Boolean)] {
      def op(a1: (Int, Int, Boolean), a2: (Int, Int, Boolean)): (Int, Int, Boolean) =
        (a1._1 min a2._1, a1._2 max a2._2, a1._3 && a2._3 && a1._2 <= a2._1)
      def zero = (Int.MinValue, Int.MinValue, true)
    }
    foldMapV(v, sortMonoid)(a => (a, a, true))._3
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // exercise 10.10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC =
      (a1, a2) match {
        case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
          Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
        case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
        case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
        case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      }
    override def zero: WC = Stub("")
  }

  // exercise 10.11
  def wordCount(words: String): Int = {
    def helper(str: String): WC = {
      if (str.length == 0) wcMonoid.zero
      else if (str.length == 1)
        if (str(0).isWhitespace) {
          Part("", 0, "")
        }
        else
          Stub(str)
      else {
        val (l, r) = str.splitAt(str.length / 2)
        wcMonoid.op(helper(l), helper(r))
      }
    }
    helper(words) match {
      case Part(l, cnt, r) => (if (l.isEmpty) 0 else 1) + cnt + (if (r.isEmpty) 0 else 1)
      case Stub(c) => if (c.isEmpty) 0 else 1
    }
  }

  def wordCount2(words: String): Int = {
    foldMapV(words, wcMonoid)(
      c => if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)
    ) match {
      case Part(l, cnt, r) => (if (l.isEmpty) 0 else 1) + cnt + (if (r.isEmpty) 0 else 1)
      case Stub(c) => if (c.isEmpty) 0 else 1
    }
  }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)

    // exercise 10.15
    def toList[A](fa: F[A]): List[A] = {
      def listMonoid[A] = new Monoid[List[A]] {
        def op(a1: List[A], a2: List[A]) = a1 ++ a2
        def zero = List()
      }
      foldMap (fa) (x => List(x))(listMonoid[A])
    }
  }

  // exercise 10.12
  class FoldableList extends Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case x :: xs => f(x, foldRight(xs)(z)(f))
      }

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Nil => z
        case x :: xs => foldLeft(xs)(f(z, x))(f)
      }

    def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      as.map(f).foldLeft(mb.zero)(mb.op)
  }

  class FoldableIndexedSeq extends Foldable[IndexedSeq] {
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as match {
        case IndexedSeq() => z
        case IndexedSeq(x, _*) => f(x, foldRight(as.tail)(z)(f))
      }

    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as match {
        case IndexedSeq() => z
        case IndexedSeq(x, _*) => foldLeft(as.tail)(f(z, x))(f)
      }

    def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      as.map(f).foldLeft(mb.zero)(mb.op)
  }

  class FoldableStream extends Foldable[Stream] {
    def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Empty => z
        case x #:: xs => f(x, foldRight(xs)(z)(f))
      }

    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Empty => z
        case x #:: xs => foldLeft(xs)(f(z, x))(f)
      }

    def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
      as.map(f).foldLeft(mb.zero)(mb.op)
  }

  // exercise 10.13
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  class FoldableTree extends Foldable[Tree] {
    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(x) => f(x, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      }

    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(x) => f(z, x)
        case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
      }

    def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case Leaf(x) => f(x)
        case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
      }
  }

  // exercise 10.14
  class FoldableOption extends Foldable[Option] {
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as match {
        case None => z
        case Some(x) => f(x, z)
      }

    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as match {
        case None => z
        case Some(x) => f(z, x)
      }

    def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case None => mb.zero
        case Some(x) => f(x)
      }
  }

  // exercise 10.16
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    def zero: (A, B) = (A.zero, B.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero){
          (acc, k) => acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
        }
    }

  // exercise 10.17
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def zero = _ => B.zero
      def op(a1: A => B, a2: A => B) = x => B.op(a1(x), a2(x))
    }

  // exercise 10.18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val M: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
    as.foldLeft(M.zero)((b: Map[A, Int], a: A) => M.op(b, Map(a -> 1)))
  }

}