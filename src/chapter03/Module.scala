package chapter03

/**
  * Created by lexun on 7/2/2016.
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) =>
      f(x, foldRight(xs, z)(f))
  }

  def sum(as: List[Int]): Int =
    foldRight(as, 0)(_ + _)

  def product(as: List[Double]): Double =
    foldRight(as, 1.0)(_ * _)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // exercise 3.3
  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(a, xs)
  }

  // exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }

  // exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else xs
  }

  def dropCurriedWhile[A](l: List[A])(f: A => Boolean): List[A] =
    dropWhile(l, f)

  // exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  // exercise 3.7
  // must use lazy evaluation
  def foldRightLazy[A, B](as: List[A], z: B)(f: (A, => B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) =>
      f(x, foldRightLazy(xs, z)(f))
  }
  def productFast(as: List[Double]): Double =
    //foldRight(as, 1.0)((a, b) => if (a == 0.0) 0.0 else a * b)
    foldRightLazy(as, 1.0)((a, b) => if (a == 0.0) 0.0 else a * b)

  // exercise 3.8
  def test_3_8: List[Int] =
    foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _))

  // exercise 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((a, b) => 1 + b)

  // exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // exercise 3.11
  def sumLeft(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def productLeft(as: List[Double]): Double =
    foldLeft(as, 1.0)(_ * _)

  def lengthLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((a, b) => a + 1)

  // exercise 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((a, b) => Cons(b, a))

  // exercise 3.13
  def foldLeftByRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a, b) => f(b, a))

  // exercise 3.14
  def append[A](as: List[A], a: A): List[A] =
    foldRight(as, Cons(a, Nil))((x, y) => Cons(x, y))

  // exercise 3.15
  def appendList[A](as: List[A], bs: List[A]): List[A] = as match {
    case Nil => bs
    case Cons(x, xs) => Cons(x, appendList(xs, bs))
  }
  def concate[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(appendList(_, _))

  // exercise 3.16
  def mapInt(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, mapInt(xs))
  }

  // exercise 3.17
  def convertListFromDoubleToString(xs: List[Double]): List[String] = xs match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, convertListFromDoubleToString(xs))
  }

  // exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  // exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }

  // exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => appendList(f(x), flatMap(xs)(f))
  }

  // exercise 3.21
  def filterbyFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  // exercise 3.22
  def addList(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addList(xs, ys))
  }

  // exercise 3.23
  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = (as, bs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  // exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???

}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // exercise 3.25
  def size[A](at: Tree[A]): Int = at match {
    case Leaf(x) => 1
    case Branch(xt, yt) => 1 + size(xt) + size(yt)
  }

  // exercise 3.26
  def maximum(at: Tree[Int]): Int = at match {
    case Leaf(x) => x
    case Branch(xt, yt) => maximum(xt) max maximum(yt)
  }

  // exercise 3.27
  def depth(at: Tree[Int]): Int = at match {
    case Leaf(x) => 1
    case Branch(xt, yt) => 1 + depth(xt) max depth(yt)
  }

  // exercise 3.28
  def map[A](at: Tree[A])(f: A => A): Tree[A] = at match {
    case Leaf(x) => Leaf(f(x))
    case Branch(xt, yt) => Branch(map(xt)(f), map(yt)(f))
  }

  // exercise 3.29
  def fold[A, B](at: Tree[A])(fl: A => B)(fb: (B, B) => B): B = at match {
    case Leaf(x) => fl(x)
    case Branch(xt, yt) => fb(fold(xt)(fl)(fb), fold(yt)(fl)(fb))
  }

  def sizeFold[A](at: Tree[A]): Int =
    fold(at)(_ => 1)((x, y) => 1 + x + y)

  def maximumFold(at: Tree[Int]): Int =
    fold(at)(x => x)(_ max _)

  def depthFold[A](at: Tree[A]): Int =
    fold(at)(_ => 1)(1 + _ max _)

  def mapFold[A](at: Tree[A])(f: A => A): Tree[A] =
    fold(at)(x => Leaf(f(x)): Tree[A])((a: Tree[A], b: Tree[A]) => Branch(a, b))
}




