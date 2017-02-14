package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  } // => 3

  def main(args: Array[String]): Unit = {
    println("Expected: 3")
    println("Actual:   %s".format(x))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil // Or sys.error("...")?
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Cons(h, Nil)
      case Cons(_, t) => Cons(h, l)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, y) => y + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}

object TestList {
  def main(args: Array[String]): Unit = {
    println(".tail: Expected: Cons(2,Nil), Nil")
    println(".tail: Actual:   %s, %s".format(List.tail(List(1, 2)), List.tail(List())))

    println(".setHead: Expected: Cons(0,Cons(1,Nil)), Cons(0,Nil)")
    println(".setHead: Actual:   %s, %s".format(List.setHead(List(1), 0), List.setHead(Nil, 0)))

    println(".drop: Expected: Cons(3,Nil), Nil, Nil")
    println(".drop: Actual:   %s, %s, %s".format(List.drop(List(1, 2, 3), 2), List.drop(List(1), 1), List.drop(List(), 1)))

    println(".dropWhile: Expected: Cons(3,Nil), Nil, Nil")
    println(".dropWhile: Actual:   %s, %s, %s".format(List.dropWhile(List(1, 2, 3), (n: Int) => n <= 2), List.dropWhile(List(1), (n: Int) => true), List.dropWhile(List(), (n: Int) => true)))

    println(".init: Expected: Cons(1,Nil), Nil, Nil")
    println(".init: Actual:   %s, %s, %s".format(List.init(List(1, 2)), List.init(List(1)), List.init(List())))

    println(".length: Expected: 2, 1, 0")
    println(".length: Actual:   %s, %s, %s".format(List.length(List(1, 2)), List.length(List(1)), List.length(List())))
  }
}