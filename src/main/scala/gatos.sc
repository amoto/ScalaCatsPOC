import cats.{Eq, Functor, Show}

import scala.::
import scala.collection.mutable.ListBuffer

val intEq = Eq.apply[Int]

intEq.eqv(1, 1)

val xEq = Eq.apply[String]

xEq.eqv("a", "b")

val mapEq = Eq.apply[Map[Int, Int]]

mapEq.eqv(Map(1 -> 1, 2 -> 3), Map(1 -> 1, 2 -> 3))

case class Gato(nombre: String)

object Gato {
  implicit val gatoEq: Eq[Gato] = (x: Gato, y: Gato) => x.equals(y)
}

val gatoEq = Eq.apply[Gato]

gatoEq.eqv(Gato("missi"), Gato("nene"))
gatoEq.eqv(Gato("nene"), Gato("nene"))
gatoEq.neqv(Gato("missi"), Gato("nene"))

val listFunctor = Functor.apply[List]

listFunctor.map(List(1, 2, 3)) ((i: Int) => i + 1 )
//val res0: List[Int] = List(2, 3, 4)

case class Contenedor[A](valor: A)
object Contenedor {
  implicit val contenedorFunctor: Functor[Contenedor] = new Functor[Contenedor] {
    override def map[A, B](fa: Contenedor[A])(f: A => B) = Contenedor(f(fa.valor))
  }
}

Functor.apply[Contenedor].map(Contenedor(1)) ((i: Int) => i+3)
//val res1: Contenedor[Int] = Contenedor(4)

object ListImplicits {
  implicit val evenListFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B) = {
      val result = ListBuffer[B]()
      for (i <- fa.indices) {
        if (i % 2 == 0) {
          result += f(fa(i))
        }
      }
      result.toList
    }
  }
}

Functor.apply[List](ListImplicits.evenListFunctor)
  .map(List(1, 2, 3, 4, 5, 6)) ((i: Int) => s"$i viene de una posicion par")
//val res0: List[String] = List(1 viene de una posicion par,
//  3 viene de una posicion par, 5 viene de una posicion par)

object StrImplicits {
  implicit val strEq: Eq[String] =
    (x: String, y: String) => x(0) == y(0) && x.last == y.last
}

Eq.apply[String](StrImplicits.strEq).eqv("oso", "oro")
//val res0: Boolean = true

implicit object ShowDouble extends Show[Double] {
  override def show(t: Double) = String.format("%.3f", t)
}

implicit object EqShowDouble extends Eq[Double]{
  override def eqv(x: Double, y: Double): Boolean = {
    val show = Show.apply[Double]

    show.show(x) == show.show(y)
  }
}

EqShowDouble.eqv(2.3456789, 2.345987)
EqShowDouble.eqv(2.3456789, 2.341987)
