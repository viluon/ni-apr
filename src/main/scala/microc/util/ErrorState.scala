package microc.util

import microc.interpreter.InterpreterState
import microc.util.ErrorState.flatMap

trait ErrorState[Error, State, +A] {
  def apply(s: State): Either[Error, (A, State)]
  def flatten[B](implicit ev: A <:< ErrorState[Error, State, B]): ErrorState[Error, State, B] = flatMap(this)(x => x)
}

object ErrorState {
  // aka return
  def pure[E, S, A](a: A): ErrorState[E, S, A] = s => Right((a, s))
  // aka bind aka >>=
  def flatMap[E, S, A, B](x: ErrorState[E, S, A])(f: A => ErrorState[E, S, B]): ErrorState[E, S, B] =
    s => x(s) match {
      case Left(err) => Left(err)
      case Right((a, s2)) => {
        (s, s2) match {
          case (state: InterpreterState, state2: InterpreterState) => {
            if (state != state2) {
              println("huh")
            } else {
              println("equal states")
            }
          }
          case _ => println("somethin's amiss")
        }
        f(a)(s2)
      }
    }

  def get[E, S]: ErrorState[E, S, S] = s => Right((s, s))
  def put[E, S](s: S): ErrorState[E, S, Unit] = _ => Right(((), s))
  def crash[E, S, A](e: E): ErrorState[E, S, A] = _ => Left(e)

  implicit class ErrorStateOps[E, S, A](x: ErrorState[E, S, A]) {
    def flatMap[B](f: A => ErrorState[E, S, B]): ErrorState[E, S, B] = ErrorState.flatMap(x)(f)
    def map[B](f: A => B): ErrorState[E, S, B] = x.flatMap(a => pure(f(a)))
    def withFilter(f: A => Boolean): ErrorState[E, S, A] = x.flatMap(a => () match {
      case _ if f(a) => pure(a)
      case _ => crash(???)
    })
  }

  implicit class ErrorStateFlattenOps[E, S, A](x: ErrorState[E, S, ErrorState[E, S, A]]) {
    def flatten: ErrorState[E, S, A] = flatMap(x)(identity)
  }
}
