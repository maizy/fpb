package ru.maizy.fpb.tests.datastructures

import org.scalatest._

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2014
 * See LICENSE.txt for details.
 */
class ListTest extends FunSuite with Matchers {

  import ru.maizy.fpb.{datastructures => ds}

  val simpleList = ds.Cons('a', ds.Cons('b', ds.Cons('c', ds.Nil)))

  test("tail") {
    assert(ds.List.tail(simpleList) === ds.Cons('b', ds.Cons('c', ds.Nil)))
    intercept[Exception] {
      ds.List.tail(ds.Nil)
    }
  }

  test("setHead") {
    assert(ds.List.setHead(simpleList, 'z') === ds.Cons('z', ds.Cons('b', ds.Cons('c', ds.Nil))))
    assert(ds.List.setHead(ds.Nil, 'z') === ds.Cons('z', ds.Nil))
  }

  test("drop") {
    assert(ds.List.drop(simpleList, 2) === ds.Cons('c', ds.Nil))
    assert(ds.List.drop(simpleList, 10) === ds.Nil)
    assert(ds.List.drop(ds.Nil, 2) === ds.Nil)
  }

  test("dropWhile") {
    assert(ds.List.dropWhile(simpleList, (_: Char) => false) === simpleList)
    assert(ds.List.dropWhile(simpleList, (x: Char) => x != 'b') === ds.Cons('b', ds.Cons('c', ds.Nil)))
    assert(ds.List.dropWhile(ds.Nil, (x: Char) => x != 'b') === ds.Nil)
  }
}
