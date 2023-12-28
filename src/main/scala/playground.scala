import scala.annotation.tailrec

object playground extends App {


  val intList = List(1, 2, 3, 4)

  val res = myFoldLeft(10)(intList) { (x1, x2) => x1 + x2 }

  println(res)

  @tailrec
  private def myFoldLeft(acc: Int)(list: List[Int])(f: (Int, Int) => Int): Int = {
    list match {
      case Nil => acc
      case ::(head, next) =>
        myFoldLeft(f(acc, head))(next)(f)
    }
  }

}
