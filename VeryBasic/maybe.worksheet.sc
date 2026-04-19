// sealed trait Maybe[+T]

sealed trait Maybe[+A]:
  def map[B](f: A => B): Maybe[B]
  def flatMap[B](f: A => Maybe[B]): Maybe[B]
  def getOrElse[B >: A](default: => B): B
  def fold[B](ifEmpty: => B)(f: A => B): B

// case class Just[+T](value: T) extends Maybe[T]
// case object Empty extends Maybe[Nothing]

case class Just[+A](value: A) extends Maybe[A]:
  def map[B](f: A => B): Maybe[B] = Just(f(value))
  def flatMap[B](f: A => Maybe[B]): Maybe[B] = f(value)
  def getOrElse[B >: A](default: => B): B = value
  def fold[B](ifEmpty: => B)(f: A => B): B = f(value)

case object Empty extends Maybe[Nothing]:
  def map[B](f: Nothing => B): Maybe[B] = Empty
  def flatMap[B](f: Nothing => Maybe[B]): Maybe[B] = Empty
  def getOrElse[B](default: => B): B = default
  def fold[B](ifEmpty: => B)(f: Nothing => B): B = ifEmpty

def dividedBy(a: Int, b: Int): Maybe[Int] =
  if b == 0 then Empty else Just(a / b)

dividedBy(10, 0)
dividedBy(10, 2)

dividedBy(10, 0).getOrElse(-1)

Just(3).fold(0)(_ * 10)
Empty.fold(0)(_ => 0)
