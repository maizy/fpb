package ru.maizy.fpb.datastructures

sealed trait MyList[+A]

case object MyNil extends MyList[Nothing]

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def sum(ints: MyList[Int]): Int = ints match {
    case MyNil => 0
    case MyCons(x, xs) => x + sum(xs)
  } 
  
  def product(ds: MyList[Double]): Double = ds match {
    case MyNil => 1.0
    case MyCons(0.0, _) => 0.0
    case MyCons(x, xs) => x * product(xs)
  }
  
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))

  val sample = MyList(1,2,3,4,5) match {
    case MyCons(x, MyCons(2, MyCons(4, _))) => x
    case MyNil => 42 
    case MyCons(x, MyCons(y, MyCons(3, MyCons(4, _)))) => x + y
    case MyCons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    a1 match {
      case MyNil => a2
      case MyCons(h,t) => MyCons(h, append(t, a2))
    }

  def tail[A](l: MyList[A]): MyList[A] =
    l match {
      case MyNil => throw new Exception("List is empty")
      case MyCons(_, xs) => xs
    }

  def setHead[A](l: MyList[A], h: A): MyList[A] =
    l match {
      case MyNil => MyCons(h, MyNil)
      case MyCons(x, xs) => MyCons(h, xs)
    }

  @annotation.tailrec
  def drop[A](l: MyList[A], n: Int): MyList[A] =
    if (n == 0 || l == MyNil) {
      l
    } else if(n < 0) {
      throw new Exception("Only positive ints allowed")
    } else {
      drop(tail(l), n - 1)
    }

  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] =
    l match {
      case MyNil => MyNil
      case MyCons(x, xs) if f(x) => dropWhile(xs, f)
      case MyCons(x, xs) => l
    }

  def init[A](l: MyList[A]): MyList[A] = {

    @annotation.tailrec
      def rec(list: MyList[A], acc: MyList[A]): MyList[A] =
        list match {
          case MyCons(x, MyNil) => MyNil
          case MyCons(x, MyCons(_, MyNil)) => append(acc, MyList(x))
          case MyCons(head, tail) => rec(tail, append(acc, MyList(head)))
          case MyNil => throw new Exception("Empty list")
        }

    rec(l, MyNil)
  }

  def length[A](l: MyList[A]): Int = sys.error("todo")

  def foldLeft[A,B](l: MyList[A], z: B)(f: (B, A) => B): B = sys.error("todo")

  def map[A,B](l: MyList[A])(f: A => B): MyList[B] = sys.error("todo")
}
