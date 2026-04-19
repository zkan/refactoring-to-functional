println("Hello world!")

val a = "123"
val b = 1
val c = 2
val result = b + c

def factorial(n: Int): Int =
  if n <= 1 then 1
  else n * factorial(n - 1)

val students1 = List("Alice", "Bob", "Charlie", "David", "Eve")
val scores1 = List(100, 90, 80, 70, 60)

def plusOne(l: List[Int]): List[Int] =
  if l.isEmpty then Nil
  else l.head + 1 :: plusOne(l.tail)

def multiplyByTwo(l: List[Int]): List[Int] =
  if l.isEmpty then Nil
  else l.head * 2 :: plusOne(l.tail)

plusOne(scores1)
multiplyByTwo(scores1)

def plus(a: Int, b: Int): Int =
  a + b

def multiply(a: Int, b: Int): Int =
  a * b

def func(l: List[Int], f: (Int, Int) => Int, o: Int): List[Int] =
  if l.isEmpty then Nil
  else f(l.head, o) :: func(l.tail, f, o)

func(scores1, multiply, 3)
