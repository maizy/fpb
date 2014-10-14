
package ru.maizy.fpb

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2014
 * See LICENSE.txt for details.
 */
object Fibonacci {
  def fib(n: BigInt): BigInt = {

    val big0 = BigInt(0)
    val big1 = BigInt(1)

    @annotation.tailrec
    def nth(i: BigInt, m2: BigInt = big0, m1: BigInt = big1): BigInt = i match {
      case `big0` => m2
      case `big1` => m1
      case _ => nth(i - big1, m1, m1 + m2)
    }

    nth(n)
  }
}


