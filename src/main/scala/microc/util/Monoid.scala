package microc.util

// instances of Monoid have this shape:
trait Monoid[A] {
  def concat(l: A, r: A): A
  def zero: A
}
