package quasiquotes.q050

private object Q050Targets:
  def empty(): List[Int] = Nil
  def one(first: Int): List[Int] = List(first)
  def many(first: Int, second: Int, third: Int): List[Int] = List(first, second, third)
  def two(first: Int, second: Int): List[Int] = List(first, second)
  def four(first: Int, second: Int, third: Int, fourth: Int): List[Int] =
    List(first, second, third, fourth)

final class Q050Constructor():
  def this(first: Int) = this()
  def this(first: Int, second: Int) = this()
  def this(first: Int, second: Int, third: Int) = this()
  def this(first: Int, second: Int, third: Int, fourth: Int) = this()
