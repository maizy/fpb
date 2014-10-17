package ru.maizy.fpb.tests.datastructures

import org.scalatest._

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2014
 * See LICENSE.txt for details.
 */
class MyListTest extends FunSuite with Matchers {

  import ru.maizy.fpb.datastructures._

  val simpleList = MyList('a', 'b', 'c')

  test("tail") {
    assert(MyList.tail(simpleList) === MyList('b', 'c'))
    intercept[Exception] {
      MyList.tail(MyNil)
    }
  }

  test("setHead") {
    assert(MyList.setHead(simpleList, 'z') === MyList('z', 'b', 'c'))
    assert(MyList.setHead(MyNil, 'z') === MyList('z'))
  }

  test("drop") {
    assert(MyList.drop(simpleList, 2) === MyList('c'))
    assert(MyList.drop(simpleList, 10) === MyNil)
    assert(MyList.drop(MyNil, 2) === MyNil)
  }

  test("dropWhile") {
    assert(MyList.dropWhile(simpleList, (_: Char) => false) === simpleList)
    assert(MyList.dropWhile(simpleList, (x: Char) => x != 'b') === MyList('b', 'c'))
    assert(MyList.dropWhile(MyNil, (x: Char) => x != 'b') === MyNil)
  }

  test("init") {
    assert(MyList.init(simpleList) === MyList('a', 'b'))
    assert(MyList.init(MyList('a')) === MyNil)
    intercept[Exception] {
      MyList.init(MyNil)
    }
  }
}
