import chapter02.Module._

fib(0)
fib(1)
fib(5)
fib(10)

val as: Array[Int] = Array(1, 2, 3, 4, 5, 6)
val bs: Array[Int] = Array(1, 2, 7, 4, 5, 6)
isSorted(as, (a: Int,b: Int) => (a < b))
isSorted(bs, (a: Int,b: Int) => (a < b))
