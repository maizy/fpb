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
}
