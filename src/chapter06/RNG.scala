package chapter06

/**
  * Created by lexun on 7/12/2016.
  */
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class State[S, +A](run: S => (A, S))  {

  def map[B](f: A => B): State[S, B] =
    State(
      ps => {
        val (nextA, nextS) = run(ps)
        (f(nextA), nextS)
      }
    )

  def map2[B, C](b: State[S, B])(f: (A, B) => C): State[S, C] =
    State(
      ps => {
        val (nextA, s1) = run(ps)
        val (nextB, s2) = b.run(s1)
        (f(nextA, nextB), s2)
      }
    )

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (nextA, nextS) = run(s)
        f(nextA).run(nextS)
      }
    )
}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](a: List[State[S, A]]): State[S, List[A]] =
    a match {
      case x :: xs => x.map2(sequence(xs))(_ :: _)
      case Nil => State(s => (Nil, s))
    }

}

// exercise 6.11
sealed trait MachineTnput
case object Coin extends MachineTnput
case object Turn extends MachineTnput

case class Machine(locked: Boolean, candies: Int, coins: Int)



