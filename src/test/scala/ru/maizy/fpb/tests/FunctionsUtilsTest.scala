package ru.maizy.fpb.tests

import org.scalatest._

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2014
 * See LICENSE.txt for details.
 */
class FunctionsUtilsTest extends FunSuite with Matchers {

  import ru.maizy.fpb.FunctionsUtils._
  test("curry / uncurry") {
    val a = (x: Int, y: Int) => x + y
    val curried = curry(a)
    val summator = curried(5)
    assert(summator(4) === 9)

    val uncurried = uncurry(curried)
    assert(uncurried(5, 2) === a(5, 2))
  }

  test("compose")  {
    val doubling = (x: String) => x + x
    val getLength = (x: String) => x.length

    val composed = compose(getLength, doubling)
    assert(composed("abc") === 6)
  }
}
