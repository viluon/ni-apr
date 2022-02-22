package microc.util

object Collections {

  implicit class ExtIterable[T](x: Iterable[T]) {
    def foreachSep[A](f: T => A, sep: => Any): Unit =
      foreachSep(f)(_ => sep)

    def foreachSep[A](f: T => A)(s: T => Any): Unit = {
      val itr = x.iterator
      while (itr.hasNext) {
        val elem = itr.next()
        f(elem)
        if (itr.hasNext) s(elem)
      }
    }
  }
}
