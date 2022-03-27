package microc.util

import microc.util.WriterState.flatMap

trait WriterState[Log, State, +A] {
  def apply(s: State): (Log, State, A)
  def flatten[B](implicit ev: A <:< WriterState[Log, State, B], mon: Monoid[Log]): WriterState[Log, State, B] =
    flatMap(this)(x => x)
}

object WriterState {
  def pure[L, S, A](x: A)(implicit mon: Monoid[L]): WriterState[L, S, A] = s => (mon.zero, s, x)
  def map[L, S, A, B](m: WriterState[L, S, A])(f: A => B): WriterState[L, S, B] =
    s => m(s) match {
      case (log, state, a) => (log, state, f(a))
    }

  @inline
  def flatMap[L, S, A, B](m: WriterState[L, S, A])(f: A => WriterState[L, S, B])(implicit mon: Monoid[L]): WriterState[L, S, B] =
    s => m(s) match {
      case (log, state, a) => f(a)(state) match {
        case (log2, state2, b) => (mon.concat(log, log2), state2, b)
      }
    }

  def foldLeft[L, S, A, B](xs: Iterable[A])(init: B)(f: (B, A) => WriterState[L, S, B])(implicit mon: Monoid[L]): WriterState[L, S, B] =
    xs.foldLeft(pure[L, S, B](init))((mb, a) => flatMap(mb)(b => f(b, a)))

  def log[L[_], Msg, S](msg: Msg)(implicit into: Into[L]): WriterState[L[Msg], S, Unit] =
    s => (into.pure(msg), s, ())

  implicit class WriterStateOps[L, S, A](m: WriterState[L, S, A]) {
    def map[B](f: A => B): WriterState[L, S, B] = WriterState.map(m)(f)
    def flatMap[B](f: A => WriterState[L, S, B])(implicit ev: Monoid[L]): WriterState[L, S, B] = WriterState.flatMap(m)(f)
    def withFilter(f: A => Boolean)(implicit ev: Monoid[L]): WriterState[L, S, A] = m.flatMap(a => () match {
      case _ if f(a) => pure(a)
      case _ => throw new IllegalStateException("fallible patterns are not allowed")
    })
  }
}
