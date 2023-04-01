package es

import u02.Optionals.*
import u03.Lists.*
import es.part1.*

object part2 extends App :

  import Option.*
  import List.*

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  def name(p: Person): String = p match
    case Person.Student(n, _) => n
    case Person.Teacher(n, _) => n

  def getCourses(l: List[Person]): List[String] = l match
    case Cons(Person.Teacher(_, c) ,t) => Cons(c, getCourses(t))
    case Cons(_, t) => getCourses(t)
    case _ => Nil()

  def foldLeft[A](l: List[A])(default: A)(f: (A, A) => A): A = l match
    case Cons(h, Nil()) => f(default, h)
    case Cons(h ,t) => foldLeft(t)(f(default, h))(f)
    case _ => default

  def foldRight[A](l: List[A])(default: A)(f: (A, A) => A): A = l match
    case Cons(h, Nil()) => f(h, default)
    case Cons(h, t) => f(h, foldRight(t)(default)(f))
    case _ => default