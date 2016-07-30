import chapter06._
import chapter06.Module._

val simpleRng = SimpleRNG(1234)

val (n, rng) = nonNegativeInt(simpleRng)
nonNegativeInt(rng)

double(simpleRng)
intDouble(simpleRng)
doubleInt(simpleRng)
double3(simpleRng)

ints(5)(simpleRng)

int(simpleRng)
nonNegativeInt(simpleRng)

intsBySequence(List.fill(5)(nonNegativeInt))(simpleRng)
intsBySequence(List.fill(5)(nonNegativeLessThan(10)))(simpleRng)
intsBySequence(List.fill(5)(nonNegativeLessThanByMap(10)))(simpleRng)
intsBySequence(List.fill(5)(nonNegativeLessThanByFlatMap(10)))(simpleRng)

type RandGeneric[+A] = State[RNG, A]
val intGeneric: RandGeneric[Int] = State(s => s.nextInt)

def intsGeneric(count: Int): RandGeneric[List[Int]] =
  State.sequence(List.fill(count)(State(s => s.nextInt)))

val nonNegtiveIntGeneric: RandGeneric[Int] =
  for {
    x <- intGeneric
  } yield Math.abs(x)

def nonNegtiveLessThanGeneric(n: Int): RandGeneric[Int] =
  nonNegtiveIntGeneric.flatMap(
    i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        State(s => (mod, s))
      else
        nonNegtiveLessThanGeneric(n)
    }
  )

//intGeneric.run(simpleRng)
//intsGeneric(5).run(simpleRng)
//nonNegtiveIntGeneric.run(simpleRng)
//nonNegtiveLessThanGeneric(10).run(simpleRng)

val ns: RandGeneric[List[Int]] =
  for {
    x <- nonNegtiveLessThanGeneric(40)
    y <- intGeneric
    xs <- intsGeneric(x)
  } yield xs.map(_ % y)
ns.run(simpleRng)






