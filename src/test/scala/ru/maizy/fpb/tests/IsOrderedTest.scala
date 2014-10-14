package ru.maizy.fpb.tests

import org.scalatest._

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2014
 * See LICENSE.txt for details.
 */
class IsOrderedTest extends FunSuite with Matchers {
  import ru.maizy.fpb.IsSorted.isSorted
  test("test ordered array") {
    val ar = Array(1, 3, 5, 8)
    assert(isSorted(ar, (x: Int, y: Int) => x < y))
    assert(!isSorted(ar, (x: Int, y: Int) => x > y))
  }

  test("test unordered array") {
    val ar = Array(1, 4, 2, 8)
    assert(!isSorted(ar, (x: Int, y: Int) => x < y))
    assert(!isSorted(ar, (x: Int, y: Int) => x > y))
  }

  test("extreme cases") {
    assert(isSorted(Array(1), (x: Int, y: Int) => false))
    assert(isSorted(Array(0, 1), (x: Int, y: Int) => x < y))
    assert(!isSorted(Array(1, 0), (x: Int, y: Int) => x < y))
  }
}
