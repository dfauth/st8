package st8

import org.apache.logging.log4j.scala.Logging

object Transition {
  def create[T,U,V](event:V, ctx:U, current:StateBuilder[T,U,V]) = TransitionBuilder[T,U,V](event, ctx, current)
}

case class Transition[T, U, V](event:V, pipeline: V => Option[State[T,U,V]], current:State[T,U,V], on_transition:Transition[T,U,V] => Transition[T,U,V]) {

  def execute(event:V): State[T,U,V] = {
    val result:Option[State[T,U,V]] = pipeline(event)
    result.foreach(next => {
      current.exit(this)
      on_transition(this)
      next.entry(this)
    })
    result.getOrElse(current)
  }
}

case class TransitionBuilder[T, U, V](event:V, ctx:U, parentBuilder:StateBuilder[T,U,V]) extends Logging {

  var next:State[T,U,V] = _
  var on_transition:Transition[T,U,V] => Transition[T,U,V] = t => t
  var guards:List[V => Boolean] = List.empty[V => Boolean]

  def build(): Transition[T,U,V] = {
    val current = parentBuilder.state()
    var pipeline:Option[State[T,U,V]] = Option[State[T,U,V]](current)
    val guard:V=>Boolean = guards.foldLeft[V=>Boolean](e=>true){
      (b:V=>Boolean, g:V=>Boolean) => e:V =>b(e) && g(e)
    }
    Transition[T,U,V](event, e =>pipeline.filter(s=>guard(e)).map(_ => next), current, on_transition)
  }

  def onTransition(f: Transition[T,U,V] => Unit):TransitionBuilder[T,U,V] = {
    this.on_transition = t=>{f(t);t}
    this
  }

  def goTo(t:T):TransitionBuilder[T,U,V] = {
    next = parentBuilder.state(t)
    this
  }

  def unless(p: U => V => Boolean):TransitionBuilder[T,U,V] = {
    guards = guards :+ p(ctx)
    this
  }
}

