package lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T

  def length: Int

  def reverse: RList[T]

  def ++[S >: T](other: RList[S]): RList[S]

  def removeAt(index: Int): RList[T]

  def map[S](f: T => S): RList[S]

  def flatMap[S](f: T => RList[S]): RList[S]

  def filter(f: T => Boolean): RList[T]

}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException()

  override def tail: RList[Nothing] = throw new NoSuchElementException()

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException()

  override def length: Int = 0

  override def reverse: RList[Nothing] = throw new NoSuchElementException()

  override def ++[S >: Nothing](other: RList[S]): RList[S] = other

  override def removeAt(index: Int): RList[Nothing] = RNil

  override def map[S](f: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T])
    extends RList[T] {
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

  override def length: Int = {
    @tailrec
    def lengthCount(currTotal: Int, remaining: RList[T]): Int = {
      if (remaining.isEmpty) currTotal
      else lengthCount(currTotal + 1, remaining.tail)
    }

    lengthCount(0, this)
  }

  override def reverse: RList[T] = {
    @tailrec
    def reverseTailRec(currentList: RList[T], remaining: RList[T]): RList[T] = {
      if (remaining.isEmpty) currentList
      else reverseTailRec(remaining.head :: currentList, remaining.tail)
    }

    reverseTailRec(RNil, this)
  }

  override def ++[S >: T](other: RList[S]): RList[S] = {
    @tailrec
    def concatTailRec(result: RList[S], remaining: RList[S]): RList[S] = {
      if (remaining.isEmpty) result
      else concatTailRec(remaining.head :: result, remaining.tail)
    }

    concatTailRec(other, this.reverse)
  }

  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeAtTailRec(
      currIndex: Int,
      currentList: RList[T],
      remaining: RList[T]
    ): RList[T] = {
      if (currIndex == index) currentList.reverse ++ remaining.tail
      else if (currIndex > index) this
      else if (remaining.isEmpty) currentList.reverse
      else {
        removeAtTailRec(
          currIndex + 1,
          remaining.head :: currentList,
          remaining.tail
        )
      }
    }

    removeAtTailRec(0, RNil, this)
  }

  override def map[S](f: T => S): RList[S] = {

    @tailrec
    def mapTailRec(acc: RList[S], remaining: RList[T]): RList[S] = {
      if (remaining.isEmpty) acc.reverse
      else mapTailRec(f(remaining.head) :: acc , remaining.tail)
    }

    mapTailRec(RNil, this)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {

    @tailrec
    def flattenTailRec(acc: RList[S], remaining: RList[RList[S]]): RList[S] = {
      if (remaining.isEmpty) acc
      else flattenTailRec(acc ++ remaining.head, remaining.tail)
    }

    flattenTailRec(RNil, this.map(f))
  }

  override def filter(f: T => Boolean): RList[T] = {

    @tailrec
    def filterTailRec(
      currentList: RList[T],
      remaining: RList[T]
    ): RList[T] = {
      if (remaining.isEmpty) currentList.reverse
      else if (f(remaining.head)) {
        filterTailRec(currentList, remaining.tail)
      } else {
        filterTailRec(remaining.head :: currentList, remaining.tail)
      }
    }

    filterTailRec(RNil, this)
  }
}

object List extends App {

  val list1 = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: RNil
  val list2 = 7 :: 8 :: 9 :: RNil

//  println(list1.removeAt(3))

  println(list1.map(x => x + 1))

  println(list1.flatMap(x => x :: (1 + x) :: RNil))

  println(list1.filter(x => x == 3))


  // List[1, 2, 3, 4, 5, 6]
  // List[1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7]
}
