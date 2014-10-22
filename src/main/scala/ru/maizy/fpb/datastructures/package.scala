package ru.maizy.fpb

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2014
 * See LICENSE.txt for details.
 */
package object datastructures {
  import MyList._

  def addOne(list: MyList[Int]) =
    foldLeft(reverse(list), MyNil: MyList[Int])((acc, x) => MyCons(x + 1, acc))

  def doubleToString(list: MyList[Double]): MyList[String] =
    foldRight(list, MyNil: MyList[String])((x, acc) => MyCons(x.toString, acc))

  def zipTwoIntLists(l: MyList[Int], l2: MyList[Int]): MyList[Int] = {
    @annotation.tailrec
    def recursive(xs: MyList[Int], xs2: MyList[Int], acc: MyList[Int]): MyList[Int] =
      xs match {
        case MyNil => acc
        case MyCons(head, tail) =>
          xs2 match {
            case MyNil => acc
            case MyCons(head2, tail2) => recursive(tail, tail2, MyCons(head + head2, acc))
          }
      }
    MyList.reverse(recursive(l, l2, MyNil))
  }

  def zipTwoIntListsNonStackSafe(l: MyList[Int], l2: MyList[Int]): MyList[Int] =
    (l, l2) match {
      case (MyNil, _) | (_, MyNil) => MyNil
      case (MyCons(head, tail), MyCons(head2, tail2)) => MyCons(head + head2, zipTwoIntListsNonStackSafe(tail, tail2))
    }

}
