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
  }

  test("foldLeft, foldRight in terms of one another") {
    // for func like produce and sum foldRight eq foldLeft (fold left better solutions for that)
    assert(MyList.foldLeft(simpleList, 0)(_ + _) === MyList.foldRight(simpleList, 0)(_ + _))
    assert(MyList.foldLeft(simpleList, 1)(_ * _) === MyList.foldRight(simpleList, 1)(_ * _))

    //in general that trick works (• - some operator)
    //    foldRight
    //    (A1 • (A2 • (A3 • (... (An • Z)...)))
    //
    //
    //    foldLeft + reverser
    //    (...(((Z • An) • An-1) ... • A1)
    //    but Z • An not the same as (An - Z)
    //    + swap args
    //    (...(A1 • (An-1 • (An • Z)))
    // but B == A in that case

    assert(
      MyList.foldRight(simpleListInt, 10)(_ - _)
      ===
      MyList.foldLeft(MyList.reverse(simpleListInt), 10)((acc: Int, x: Int) => x - acc)
    )

    //there is another not stack-safe implementation in answers, when you build stack of funcs
    // and the call them with init value
    // (f() => f() => f())(z)
  }

  test("foldLeft based funcs") {
    assert(MyList.lenght3(simpleList) === 3)
    assert(MyList.sum3(simpleListInt) === MyList.sum(simpleListInt))
    assert(MyList.product3(simpleListDouble) === MyList.product(simpleListDouble))
  }

  test("reverse") {
    assert(MyList.reverse(simpleList) === MyList('c', 'b', 'a'))
    assert(MyList.reverse(MyNil) === MyNil)
  }

  test("append 2") {
    assert(MyList.append(simpleList, MyList('x', 'y', 'z')) === MyList('a', 'b', 'c', 'x', 'y', 'z'))
  }

  test("flatten") {
    val data1 = MyList(
      MyList('a', 'b'),
      MyNil,
      MyList('c'),
      MyNil
    )
    val expected1 = MyList('a', 'b', 'c')
    assert(
      MyList.flattenStackSafe(
        data1
      )
      ===
      expected1
    )

    assert(
      MyList.flattenLinear(
        data1
      )
      ===
      expected1
    )

    val data2 = MyList(
      MyNil,
      MyNil
    )
    assert(
      MyList.flattenStackSafe(
        data2
      )
      ===
      MyNil
    )
    assert(
      MyList.flattenLinear(
        data2
      )
      ===
      MyNil
    )
  }

  test("map") {
    assert(MyList.map(simpleListInt)(_ + 1) === MyList(2, 3, 4, 5))
  }

  test("filter") {
    assert(MyList.filter(simpleListInt)(_ % 2 == 0) === MyList(2, 4))
  }
}
