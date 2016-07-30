import chapter03._

val ls = List(1, 2, 3, 4, 5, 6)
List.tail(ls)
List.setHead(ls, 10)
List.drop(ls, 3)
List.dropWhile(ls, (x: Int) => x < 4)
List.init(ls)

List.dropCurriedWhile(ls)(_ < 4)

val is = List(1, 2, 3, 4, 5)
val ds = List(1.0, 2.0, 3.0, 4.0, 5.0)
val zs = List(1.0, 2.0, 3.0, 4.0, 0.0, 5.0)
List.sum(is)
List.product(ds)
List.product(zs)
List.productFast(zs)

List.test_3_8

List.length(is)

List.sumLeft(is)
List.productLeft(ds)
List.lengthLeft(is)

List.reverse(ds)

List.foldLeftByRight(is, 0)(_ + _)

List.append(List(1, 2, 3, 4), 5)

List.concate(List(
  List(1, 2, 3, 4, 5),
  List(6, 7, 8),
  List(9, 10, 11, 12, 13),
  List(14, 15),
  List(16)
))

List.flatMap(List(1, 2, 3))(i => List(i, i))
List.filterbyFlatMap(List(1, 2, 3, 4, 5, 6))(_ < 3)

List.addList(List(1, 2, 3), List(4, 5, 6, 7))

val tree =
  Branch(
    Branch(
      Branch(
        Leaf(9),
        Branch(
          Leaf(2),
          Leaf(1)
        )
      ),
      Branch(
        Branch(
          Leaf(45),
          Leaf(47)
        ),
        Leaf(10)
      )
    ),
    Branch(
      Leaf(3),
      Branch(
        Leaf(5),
        Branch(
          Leaf(3),
          Leaf(12)
        )
      )
    )
  )

Tree.size(tree)
Tree.maximum(tree)
Tree.depth(tree)
Tree.map(tree)(_ * 2)

Tree.sizeFold(tree)
Tree.maximumFold(tree)
Tree.depthFold(tree)
Tree.mapFold(tree)(_ * 2)

