package ru.maizy.fpb.tests.datastructures

import org.scalatest._

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2014
 * See LICENSE.txt for details.
 */
class PackageTest extends FunSuite with Matchers {

  import ru.maizy.fpb.datastructures._

  val simpleList = MyList('a', 'b', 'c')
  val simpleListInt = MyList(1, 2, 3, 4)
  val simpleListDouble = MyList(1.0, 2.0, 3.0, 4.0)

  test("addOne") {
    assert(addOne(simpleListInt) === MyList(2, 3, 4, 5))
  }

  test("double to string") {
    assert(doubleToString(simpleListDouble) === MyList("1.0", "2.0", "3.0", "4.0"))
  }

  test("zipTwoIntLists") {
    assert(zipTwoIntLists(simpleListInt, MyList(3, 4)) === MyList(4, 6))
  }
}
