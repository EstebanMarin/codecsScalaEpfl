case class Rational(numer: Int, denum: Int)

trait Ordering[A]:
  def compare(x: A, y: A): Boolean

given RationalOrdering: Ordering[Rational] with
  def compare(x: Rational, y: Rational) =
    val xn = x.numer * y.denum
    val yn = y.numer * x.denum
    if xn < yn then -1 else if xn > yn then 1 else 0
    ???

trait Semigroup[T]:
  extension (x: T) def combine(y: T): T

trait Monoid[T] extends Semigroup[T]:
  def unit: T
object Monoid:
  def apply[T](using m: Monoid[T]): Monoid[T] = m

def reduce[T](xs: List[T])(using m: Semigroup[T]): T =
  xs.reduce(_.combine(_))

def reduceMonoids[T](xs: List[T])(using m: Monoid[T]): T =
  xs.foldLeft(m.unit)(_.combine(_))

def reduceMonoidsBounds[T: Monoid](xs: List[T]): T =
  xs.foldLeft(summon[Monoid[T]].unit)(_.combine(_))

def reduceMonoidsBounds2[T: Monoid](xs: List[T]): T =
  xs.foldLeft(Monoid[T].unit)(_.combine(_))

given sumMonoid: Monoid[Int] with
  extension (x: Int) def combine(y: Int): Int = x + y
  def unit = 0

given productMonoid: Monoid[Int] with
  extension (x: Int) def combine(y: Int): Int = x * y
  def unit = 1

def sum(xs: List[Int]) = reduce(xs)(using sumMonoid)
def prod(xs: List[Int]) = reduce(xs)(using productMonoid)

val xs = List(1,2,3,4)
sum(xs)
prod(xs)