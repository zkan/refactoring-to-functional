import scala.compiletime.ops.string
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

def funcCurrying(f: (Int, Int) => Int)(o: Int)(l: List[Int]): List[Int] =
  if l.isEmpty then Nil
  else f(l.head, o) :: funcCurrying(f)(o)(l.tail)

val plusOneFunc = funcCurrying(plus)(1)
plusOneFunc(scores1)

def myPrintln(s: String): Unit =
  println(s)

// Func ถูก executed ตรงนี้ ซึ่งถ้าเราเอา currying มาช่วย เราจะควบคุมได้ว่าเราจะไป execute ตรงไหน
val x = myPrintln("Hello")
val y = x

def plus1(a: Int): Int =
  a + 1

extension (l: List[Int])
  def transform(f: Int => Int): List[Int] =
    if l.isEmpty then Nil
    else f(l.head) :: l.tail.transform(f)

// จริง ๆ ตรงนี้ใช้ for loop ก็ได้ แต่ for loop จะไม่ได้บอก intention ที่ชัดเจน (เราต้องเข้าไปอ่านโค้ดใน loop เอง)
scores1.transform(plus1)

def sum(a: Int, b: Int): Int =
  a + b

extension (l: List[Int])
  def myReduce(f: (Int, Int) => Int): Int =
    l match
      case Nil    => throw new UnsupportedOperationException("empty.reduce")
      case h :: t =>
        t match
          case Nil => h
          case _   => f(h, t.myReduce(f))

scores1.myReduce(sum)

final class IO[A] private (val unsafeRun: () => A):

  // Transform the result without running the effect
  def map[B](f: A => B): IO[B] =
    IO(f(unsafeRun()))

  // Chain a computation that itself produces an IO
  def flatMap[B](f: A => IO[B]): IO[B] =
    IO(f(unsafeRun()).unsafeRun())

object IO:
  // Wrap a pure value — no side effects
  def pure[A](value: A): IO[A] =
    IO(value)

  // Wrap a side-effecting block, keeping it suspended
  def apply[A](block: => A): IO[A] =
    new IO(() => block)

val readName: IO[String] =
  IO:
    print("Enter your name: ")
    scala.io.StdIn.readLine()

val greet: IO[Unit] =
  readName.flatMap: name =>
    IO(println(s"Hello, $name!"))

// for-comprehension desugars to flatMap/map — idiomatic Scala 3
val pipeline: IO[Unit] =
  for
    _ <- IO(println("--- IO Monad Demo ---"))
    _ <- greet
    _ <- IO(println("Done."))
  yield ()

// Nothing has run yet — execute the whole pipeline here
// pipeline.unsafeRun()
