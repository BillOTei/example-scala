package billo.fpinscala

import billo.fpinscala.{List => newList}

object Example {
  def main (args: Array[String]) = {
    /*println(isSorted(
      Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97),
      (x: Int, y: Int) => y > x
    ))*/

    //val concatIncrement = partial1(1, (n: Int, s: String) => s + n)

    /** Result: 3 */
    /*val x = newList(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + newList.sum(t)
      case _ => 101
    }*/

    val l = newList(1.0, 2.54, 3.0, 87.987, 465.0)
    val l2 = newList(87, 44, 75, "dfg")
    val l4 = newList(1, 2, 3, 54)
    val l3 = newList(40, 50, 60)

    println(newList.addIntLists(l4, l3))
  }

  def winner(p1: Player, p2: Player): Player =
    if (p1.score < p2.score) p1 else p2

  def abs(n: Int): Int =
    if (n < 0) -n else n

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  /** Just a recursive factorial */
  def factorial(n: Int): Int = {
    def go(m: Int, acc: Int): Int =
      if (m < 1) acc else go(m - 1, m * acc)

    go(n, 1)
  }

  /** Just a recursive Fibonacci */
  def fibonacci(n: Int): Int = {
    def go(n: Int, m: Int, acc: Int): Int =
      if (n == 0) m else go(n - 1, acc, acc + m)

    go(n, 0, 1)
  }

  /** Bad binary search implementation */
  def binarySearch(s: Array[Double], key: Double): Int = {
    def go(low: Int, mid: Int, high: Int): Int = {
      if (!mid.isInstanceOf[Int]) -1
      else if (mid < low) go(low, low, high)
      else if (s(mid) > key) go(low, mid / 2, mid - 1)
      else if (s(mid) < key) go(mid + 1, high / 2, high)
      else mid
    }

    go(0, (s.length - 1) / 2, s.length - 1)
  }

  /** Is an array of A sorted */
  def isSorted[A](s: Array[A], f: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(low: Int, high: Int): Boolean = {
      if (low >= s.length - 1) true
      else if (f(s(high), s(low))) false
      else go(high, high + 1)
    }

    go(0, 1)
  }

  /** Partial function application */
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  /** Currying */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  /** Compose */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}

case class Player(name: String, score: Double)

