import chapter05.Stream
import chapter05.Module._

val s = Stream(1, 2, 3, 4, 5, 6, 7, 8)
s.toList
s.take(4).toList
s.drop(4).toList
s.takeWhile(x => x <= 3).toList

s.exists(x => x == 3)
s.exists(x => x == 9)
s.forAll(x => x> 0 && x < 9)
s.forAll(x => x > 7)
s.takeWhileRight(x => x <= 3).toList

s.map(x => x * 2).toList
s.filter(x => x % 2 == 0).toList
s.append(Stream(12, 13)).toList
s.flatMap(x => Stream(x * 2)).toList

ones.take(5).toList
ones.exists(x => x % 2 != 0)

constant(5).take(4).toList
from(4).take(5).toList
fibs.take(10).toList

onesUnfold.take(5).toList
onesUnfold.exists(x => x % 2 != 0)

constantUnfold(5).take(4).toList
fromUnfold(4).take(5).toList
fibsUnfold.take(10).toList

s.mapUnfold(x => x * 2).toList
s.takeUnfold(4).toList
s.takeWhileUnfold(x => x <= 3).toList
