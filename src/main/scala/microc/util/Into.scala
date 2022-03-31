package microc.util

trait Into[M[_]] {
  def pure[A](x: A): M[A]
}
