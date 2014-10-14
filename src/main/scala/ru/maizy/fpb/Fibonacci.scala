
package ru.maizy.fpb

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2014
 * See LICENSE.txt for details.
 */
object Fibonacci {
  def fib(n: Int): Int = {

    // 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144

    @annotation.tailrec
    def nth(i: Int, m2: Int, m1: Int): Int = i match {
      case `n` => m1 + m2
      case _ => nth(i + 1, m1, m1 + m2)
    }

    n match {
      case 0 => 0
      case 1 => 1
      case _ if n < 0 => throw new Exception("not supported")
      case _ => nth(2, fib(0), fib(1))
    }
  }
}


