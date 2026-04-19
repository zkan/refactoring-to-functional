package foundation.level2
//sbt "runMain foundations.runRecursion"
import scala.annotation.tailrec
import scala.annotation.targetName

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

@main def runRecursion(): Unit =
  println(factorial(5))
  println(plusOne(scores1))
  println(multiplyByTwo(scores1))
