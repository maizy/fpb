package ru.maizy.fpb

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2014
 * See LICENSE.txt for details.
 */
object IsSorted {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @annotation.tailrec
    def check(xs: Array[A], x: A): Boolean = {
      xs.length match {
        case 0 => true
        case _ if !ordered(x, xs.head) => false
        case _ => check(xs.tail, xs.head)
      }
    }

    as.length match {
      case 0 | 1 => true
      case _ => check(as.tail, as.head)
    }
  }
}
