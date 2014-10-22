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

  def append2[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    foldLeft(reverse(a1), a2)((acc, x) => MyCons(x, acc))

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

  // --

  def foldRight[A,B](as: MyList[A], z: B)(f: (A, B) => B): B =
    as match {
      case MyNil => z
      case MyCons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: MyList[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: MyList[Double]) =
    foldRight(ns, 1.0)(_ * _)


  def length[A](list: MyList[A]): Int =
    foldRight(list, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](list: MyList[A], z: B)(f: (B, A) => B): B =
    list match {
      case MyNil => z
      case MyCons(x, tail) => foldLeft(tail, f(z, x))(f)
    }

  def sum3(list: MyList[Int]): Int =
    foldLeft(list, 0)(_ + _)

  def product3(list: MyList[Double]): Double =
    foldLeft(list, 1.0)(_ * _)
  
  def lenght3[A](list: MyList[A]): Int =
    foldLeft(list, 0)((acc, _) => acc + 1)

  def reverse[A](list: MyList[A]): MyList[A] =
    foldLeft(list, MyNil: MyList[A])((acc: MyList[A], x: A) => MyCons(x, acc))  //N.B implicit type of MyNil

  def map[A,B](l: MyList[A])(f: A => B): MyList[B] =
    foldRight(l, MyNil: MyList[B])((x, acc) => MyCons(f(x), acc))

  def flattenStackSafe[A](l: MyList[MyList[A]]): MyList[A] =
    foldLeft(l, MyNil: MyList[A])((acc, lst) => append2(acc, lst))

  def flattenLinear[A](l: MyList[MyList[A]]): MyList[A] =
    foldRight(l, MyNil: MyList[A])(append)

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    foldRight(as, MyNil: MyList[A])((x, acc) => if (f(x)) MyCons(x, acc) else acc)

  def flatMap[A,B](as: MyList[A])(f: A => MyList[B]): MyList[B] =
    flattenLinear(map(as)(f))

}
