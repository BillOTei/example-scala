package billo.fpinscala

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def prodFoldLeft(ints: List[Double]): Double = foldLeft(ints, 1.0)(_ * _)

  def lengthFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((rl, h) => Cons(h, rl))

  def append[A](l: List[A], x: List[A]): List[A] = foldRight(l, x)(Cons(_, _))

  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)

  def add1(l: List[Int]): List[Int] = reverse(foldLeft(l, List[Int]())((acc, h) => Cons(h + 1, acc)))

  def add1Bis(l: List[Int]): List[Int] = foldRight(l, List[Int]())((h, acc) => Cons(h + 1, acc))

  def doubleToString(l: List[Double]) = foldRight(l, Nil:List[String])((h, acc) => Cons(h.toString, acc))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((h, acc) => Cons(f(h), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil:List[A])((h, acc) => if (f(h)) Cons(h, acc) else acc)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, Nil:List[B])((h, acc) => append(f(h), acc))

  def filterFromFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(h => if (f(h)) List(h) else Nil)

  def hasSubsequence[A, B](l: List[A], sub: List[A]): Int = foldLeft(containsValues(l, sub), -1)(condSum)

  def condSum(acc: Int, a: Int) = if (acc <= 0) a else if (acc > 0 && a != 0) acc + a else if (acc > 0 && a == 0) 0 else acc

  def startsWith[A, B](l: List[A], sub: List[A]): Boolean = l match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }

  /** Solution ex 24 */
  @annotation.tailrec
  def hasSubsequenceSolution[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequenceSolution(t, sub)
  }

  def containsValues[A, B](l: List[A], sub: List[A]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(foldLeft(sub, 0)((z, h2) => if (z == 0 && h2 == h) 1 else z), containsValues(t, sub))
  }

  def addIntLists(l: List[Int], m: List[Int]): List[Int] = (l, m) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addIntLists(t1, t2))
  }

  def zipWith[A, B](l: List[A], m: List[A])(f: (A, A) => B): List[B] = (l, m) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def drop[A](n: Int, l: List[A]): List[A] = {
    if (n <= 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(n - 1, t)
      }
    }
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }
  }

  def setHead[A](v: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("setHead of empty list")
    case Cons(_, t) => Cons(v, t)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)
}