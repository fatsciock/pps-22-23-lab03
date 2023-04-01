package es

import org.junit.*
import org.junit.Assert.*
import u02.Optionals.*
import u03.Lists.*
import es.part1.*
import es.part2.*

class part2Test {
  import Option.*
  import List.*

  val listaPersone = Cons(Person.Student("Andrea", 1998), Cons(Person.Teacher("Mirko", "OOP"), Cons(Person.Teacher("Alessandro", "PCD"), Nil())))
  val lst = Cons (3, Cons (7, Cons (1, Cons (5, Nil ()))))

  @Test def testGetCourses() =
    assertEquals(Cons("OOP", Cons("PCD", Nil())), getCourses(listaPersone))

  @Test def testFoldLeft() =
    assertEquals(-16, foldLeft(lst)(0)(_ - _))

  @Test def testFoldRight() =
    assertEquals(-8, foldRight(lst)(0)(_ - _))
}
