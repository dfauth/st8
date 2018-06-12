package st8

import org.apache.logging.log4j.scala.Logging

object Transition {
  def create[T,U,V](ctx:U, current:State[T,U,V]) = Builder[T,U,V](ctx, current)
}

case class Transition[T, U, V](pipeline: () => Option[State[T,U,V]], current:State[T,U,V], on_transition:Transition[T,U,V] => Transition[T,U,V]) {

  def execute(): State[T,U,V] = {
    val result:Option[State[T,U,V]] = pipeline()
    result.foreach(o => {
      on_transition(this)
    })
    result.getOrElse(current)
  }
}

case class Builder[T, U, V](ctx:U, current:State[T,U,V]) extends Logging {

  var next:State[T,U,V] = _
  var on_transition:Transition[T,U,V] => Transition[T,U,V] = t => {
    logger.info("identity()")
    t
  }
  var guard:U => State[T,U,V] => Boolean = _
  var pipeline:Option[State[T,U,V]] = Option[State[T,U,V]](current)

  def build(): Transition[T,U,V] = {
    Transition[T,U,V](() =>pipeline.filter(guard(ctx)).map(_ => next), current, on_transition)
  }

  def onTransition(f: Transition[T,U,V] => Unit):Builder[T,U,V] = {
    this.on_transition = t=>{
      logger.info("here: "+t)
      f(t);
      t
    }
    this
  }

  def goTo(state:T):Builder[T,U,V] = {
    next = State[T,U,V](ctx, state)
    this
  }

  def unless(p: U => State[T,U,V] => Boolean):Builder[T,U,V] = {
    guard = p
    this
  }
}

