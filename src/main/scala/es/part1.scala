package es

import u02.Optionals.*
import u03.Lists.*

object part1 extends App :

  import Option.*
  import List.*

  def drop[A](l: List[A], n: Int): List[A] = l match
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case Cons(h, t) if n == 0 => Cons(h, t)
    case _ => Nil()

  def append[A](left: List[A], right: List[A]): List[A] = left match
    case Cons(h, t) => Cons(h, append(t, right))
    case _ => right

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
    case Cons(h, Nil()) => f(h)
    case Cons(h, t) => append(f(h), flatMap(t)(f))
    case _ => Nil()

  def mapTroughFlatMap[A, B](l: List[A])(mapper: A => B): List[B] =
    flatMap(l)(v => Cons(mapper(v), Nil()))

  def filterTroughFlatMap[A](l1: List[A])(pred: A => Boolean): List[A] =
    flatMap(l1)(v => v match
      case v if pred(v) => Cons(v, Nil())
      case _ => Nil())

  def max(l: List[Int]): Option[Int] = l match
    case Cons(h1, Cons(h2, t)) if h1 > h2 => max(append(Cons(h1, Nil()), t))
    case Cons(h1, Cons(h2, t)) if h1 < h2 => max(Cons(h2, t))
    case Cons(h, Nil()) => Some(h)
    case _ => None()
