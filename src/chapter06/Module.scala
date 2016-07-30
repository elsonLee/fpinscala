package chapter06

/**
  * Created by lexun on 7/12/2016.
  */
object Module {

  // exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextN, nextRNG) = rng.nextInt
    if (nextN > 0) (nextN, nextRNG)
    else nonNegativeInt(nextRNG)
  }

  // exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (nextN, nextRNG) = rng.nextInt
    if (nextN < 0 || nextN == Int.MaxValue) double(nextRNG)
    else (nextN.toDouble / Int.MaxValue, nextRNG)
  }

  // exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nextInt, rng1) = rng.nextInt
    val (nextDouble, rng2) = double(rng1)
    ((nextInt, nextDouble), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((nextInt, nextDouble), nextRNG) = intDouble(rng)
    ((nextDouble, nextInt), nextRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (n1, rng1) = double(rng)
    val (n2, rng2) = double(rng1)
    val (n3, rng3) = double(rng2)
    ((n1, n2, n3), rng3)
  }

  // exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (Nil, rng)
    else {
      val (nextN, nextRNG) = rng.nextInt
      val nextList = ints(count - 1)(nextRNG)
      (nextN :: nextList._1, nextList._2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](a: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (nextA, nextRNG) = a(rng)
      (f(nextA), nextRNG)
    }

  def nonNegtiveEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // exercise 6.5
  def doubleByMap: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  // exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (nextA, rng1) = ra(rng)
      val (nextB, rng2) = rb(rng1)
      (f(nextA, nextB), rng2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  // exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs match {
      case x :: xs => map2(x, sequence(xs))(_ :: _)
      case Nil => rng => (Nil, rng)
    }

  def intsBySequence(fs: List[Rand[Int]]): Rand[List[Int]] =
    sequence(fs)

  def nonNegativeLessThanByMap(n: Int): Rand[Int] =
    map(nonNegativeInt)(_ % n)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    rng => {
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        (mod, rng2)
      else nonNegativeLessThan(n)(rng)
    }

  // exercise 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (nextA, nextRNG) = f(rng)
      g(nextA)(nextRNG)
    }

  def nonNegativeLessThanByFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){
      i => {
        val mod = i % n
        if (i + (n - 1) - mod >= 0)
          rng => (mod, rng)
        else
          nonNegativeLessThanByFlatMap(n)
      }
    }

  // exercise 6.9
  def mapByFlatMap[A, B](a: Rand[A])(f: A => B): Rand[B] =
    flatMap(a)(x => rng => (f(x), rng))

  def map2ByFlatMap[A, B, C](a: Rand[A], b: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(a)(x => flatMap(b)(y => rng => (f(x, y), rng)))

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def simulateMachine(inputs: List[MachineTnput]): State[(Int, Int), Machine] = ???

}
