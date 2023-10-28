package lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException()

  override def tail: RList[Nothing] = throw new NoSuchElementException()

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException()
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head},")
    }
    "[" + toStringTailrec(this, "") + "]"
  }

  override def apply(index: Int): T = {
    find(index, this)
  }

  @tailrec
  private def find[S >: T](index: Int, remaining: RList[S]): S = {
    if (index == 0) remaining.head
    else if (remaining.isEmpty) throw new NoSuchElementException()
    else find(index - 1, remaining.tail)
  }
}

object List extends App {

  val list = 1 :: 2 :: 3 :: 4 ::  RNil
  println(list(3))

}