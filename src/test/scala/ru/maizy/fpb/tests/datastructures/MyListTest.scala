package ru.maizy.fpb.tests.datastructures

import org.scalatest._

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2014
 * See LICENSE.txt for details.
 */
class MyListTest extends FunSuite with Matchers {

  import ru.maizy.fpb.datastructures._

  val simpleList = MyList('a', 'b', 'c')
  val simpleListInt = MyList(1, 2, 3, 4)
  val simpleListDouble = MyList(1.0, 2.0, 3.0, 4.0)

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

    val bookExample = MyList(1,2,3,4,5)
    assert(MyList.dropWhile(bookExample, (x: Int) => x < 4) === MyList(4, 5))
  }

  test("init") {
    assert(MyList.init(simpleList) === MyList('a', 'b'))
    assert(MyList.init(MyList('a')) === MyNil)
    intercept[Exception] {
      MyList.init(MyNil)
    }
  }

  test("length") {
    assert(MyList.length(simpleList) === 3)
    assert(MyList.length(MyNil) === 0)
  }

  test("foldLeft, foldRight in terms of foldleft") {
    assert(MyList.foldLeft(simpleListInt, 0)(_ + _) === 10)
    assert(MyList.foldLeft(simpleListInt, 1)(_ * _) === 24)
    assert(MyList.foldLeft(simpleList, "")(_ + _) === "abc")

    // for func like produce and sum foldRight eq foldLeft (fold left better)
    assert(MyList.foldLeft(simpleList, 0)(_ + _) === MyList.foldRight(simpleList, 0)(_ + _))
    assert(MyList.foldLeft(simpleList, 1)(_ * _) === MyList.foldRight(simpleList, 1)(_ * _))

    // but ...
    assert(MyList.foldLeft(simpleList, 10)(_ - _) !== MyList.foldRight(simpleList, 10)(_ - _))

    // for left assotiative func it's equivalent for foldRight
    assert(List(1,2,3).foldRight(10)(_ - _) !== List(1,2,3).reverse.foldLeft(10)(_ - _))

    // for right associative func not
    assert(List("a","b","c").foldRight("")(_ + _) !== List("a","b","c").reverse.foldLeft("")(_ + _))
  }

  test("foldLeft based funcs") {
    assert(MyList.lenght3(simpleList) === 3)
    assert(MyList.sum3(simpleListInt) === MyList.sum(simpleListInt))
    assert(MyList.product3(simpleListDouble) === MyList.product(simpleListDouble))
  }
}
