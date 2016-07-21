import chapter10.Monoid
import chapter10.Monoid._

val arr: IndexedSeq[Int] = IndexedSeq(1, 2, 3)
arr.splitAt(0)

foldMapV((1 to 100).toIndexedSeq, intAddition)(x => x * x)

isSorted(IndexedSeq(-4, 3, 5, 2, 8, 9))
isSorted(IndexedSeq(-4, 8, 9, 7, 18, 19))
isSorted(IndexedSeq(-4, 3, 5, 6, 8, 9))

wordCount("Hello this is a sentence")
wordCount2("Hello this is a sentence now")

val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))
val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
val m2 = Map("o1" -> Map("i2" -> 3))
val m3 = M.op(m1, m2)

bag(IndexedSeq("a", "rose", "is", "a", "rose"))

val m = productMonoid(intAddition, intAddition)
val listFoldable = new FoldableList
listFoldable.foldMap(List(1, 2, 3, 4, 5))(a => (1, a))(m)
listFoldable.foldMap(List(1, 2, 3, 4, 5))(a => (1, a * a))(m)
