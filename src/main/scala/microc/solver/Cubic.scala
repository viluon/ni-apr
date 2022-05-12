package microc.solver

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Cubic {
  sealed trait Constraint[T, V]

  /**
    * Represents a t ∈ x constraint.
    */
  case class In[T, V](t: T, x: V) extends Constraint[T, V]

  /**
    * Represents a t ∈ x ⇒ y ⊆ z constraint (where t ∈ x comes from cond).
    */
  case class CondSubSet[T, V](cond: In[T, V], y: V, z: V) extends Constraint[T, V]
}

case class Cubic[T, V](tokens: Set[T], variables: Set[V], constraints: List[Cubic.Constraint[T, V]]) {
  type Graph = mutable.Map[V, ConstrVar]

  /**
    * Constraint variable.
    */
  case class ConstrVar(v: V, sol: mutable.Set[T], succ: mutable.Set[V], cond: mutable.Map[T, ListBuffer[(V, V)]])

  private def empty(v: V): ConstrVar = ConstrVar(v, mutable.Set(), mutable.Set(), mutable.Map((for (t <- tokens) yield t -> ListBuffer[(V, V)]()).toSeq: _*))
  private def initial: Graph = mutable.Map((for (v <- variables) yield v -> empty(v)).toSeq: _*)

  def solve(): Graph = {
    val worklist: ListBuffer[(T, ConstrVar)] = ListBuffer()
    val graph = initial

    def addToken(t: T, x: ConstrVar): Unit = {
      if (!x.sol.contains(t)) {
        x.sol.addOne(t)
        worklist.append((t, x))
      }
    }

    def addEdge(x: ConstrVar, y: ConstrVar): Unit = {
      if (x != y && !x.succ.contains(y.v)) {
        x.succ.addOne(y.v)
        for (t <- x.sol) addToken(t, y)
      }
    }

    def propagate(): Unit = {
      while (worklist.nonEmpty) {
        val (t, x) = worklist.remove(0)
        for ((y, z) <- x.cond(t)) addEdge(graph(y), graph(z))
        for (y <- x.succ) addToken(t, graph(y))
      }
    }

    for (constr <- constraints) constr match {
      case Cubic.In(t, x) =>
        addToken(t, graph(x))
        propagate()
      case Cubic.CondSubSet(Cubic.In(t, x), y, z) =>
        if (graph(x).sol.contains(t)) {
          addEdge(graph(y), graph(z))
          propagate()
        } else graph(x).cond(t).append((y, z))
    }

    graph
  }
}
