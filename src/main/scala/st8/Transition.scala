package st8

import org.apache.logging.log4j.scala.Logging

object Transition {
  def create[T,U,V](event:V, ctx:U, current:StateBuilder[T,U,V]) = TransitionBuilder[T,U,V](event, ctx, current)
}

case class Transition[T, U, V](event:V, pipeline: () => Option[State[T,U,V]], current:State[T,U,V], on_transition:Transition[T,U,V] => Transition[T,U,V]) {

  def execute(): State[T,U,V] = {
    val result:Option[State[T,U,V]] = pipeline()
    result.foreach(o => {
      on_transition(this)
    })
    result.getOrElse(current)
  }
}

case class TransitionBuilder[T, U, V](event:V, ctx:U, parentBuilder:StateBuilder[T,U,V]) extends Logging {

  var next:State[T,U,V] = _
  var on_transition:Transition[T,U,V] => Transition[T,U,V] = t => {
    logger.info("identity()")
    t
  }
  var guard:U => State[T,U,V] => Boolean = _

  def build(): Transition[T,U,V] = {
    val current = parentBuilder.state()
    var pipeline:Option[State[T,U,V]] = Option[State[T,U,V]](current)
    Transition[T,U,V](event, () =>pipeline.filter(guard(ctx)).map(_ => next), current, on_transition)
  }

  def onTransition(f: Transition[T,U,V] => Unit):TransitionBuilder[T,U,V] = {
    this.on_transition = t=>{
      logger.info("here: "+t)
      f(t);
      t
    }
    this
  }

  def goTo(t:T):TransitionBuilder[T,U,V] = {
    next = parentBuilder.state(t)
    this
  }

  def unless(p: U => State[T,U,V] => Boolean):TransitionBuilder[T,U,V] = {
    guard = p
    this
  }
}

