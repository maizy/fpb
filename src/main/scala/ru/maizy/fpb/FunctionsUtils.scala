package ru.maizy.fpb

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2014
 * See LICENSE.txt for details.
 */
object FunctionsUtils {
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (x: A) => (y: B) => f(x, y)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (x: A, y: B) => f(x)(y)

}
