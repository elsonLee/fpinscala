import chapter04.errHandling._
import chapter04.Option
import chapter04.Either
//import chapter04.EitherList

sequence(List(Some(1), Some(2), Some(3), Some(4)))
sequence(List(Some(1), Some(2), None, Some(4)))

parseInts(List("53234"))
parseInts(List("5a3234"))
parseIntsFast(List("53234"))
parseIntsFast(List("5a3234"))

sequenceEither(List(Right(1), Right(2), Right(3), Right(4)))
sequenceEither(List(Right(1), Right(2), Left("catch you"), Right(4)))

parseIntsFastEither(List("53234"))
parseIntsFastEither(List("5a3234"))
