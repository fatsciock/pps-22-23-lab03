package es

import org.junit.*
import org.junit.Assert.*
import u02.Optionals.*
import u03.Lists.*
import es.part1.*


class part1Test {
  import Option.*
  import List.*

  val lst: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val tail = Cons (40 , Nil ())

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(lst, 1))
    assertEquals(Cons(30, Nil()), drop(lst, 2))
    assertEquals(Nil(), drop(lst, 5))

  @Test def testAppend() =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil ())))), append (lst , tail ))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), part1.flatMap(lst)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), part1.flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil ()))))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapTroughFlatMap(lst)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapTroughFlatMap(lst)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filterTroughFlatMap(lst)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterTroughFlatMap(lst)(_ != 20))

  @Test def testMax() =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(None(), max(Nil()))

}
