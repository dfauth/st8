package st8

object Transition {
  def create[T,U,V](ctx:U, current:State[T,U,V]) = Builder[T,U,V](ctx, current)
}

case class Transition[T, U, V](pipline: () => Option[State[T,U,V]], current:State[T,U,V]) {

  def execute(): State[T,U,V] = {
    pipline().getOrElse(current)
  }
}

case class Builder[T, U, V](ctx:U, current:State[T,U,V]) {

  var next:State[T,U,V] = _
  var on_transition:T => Unit = _
  var guard:U => State[T,U,V] => Boolean = _
  var pipeline:Option[State[T,U,V]] = Option[State[T,U,V]](current)

  def build(): Transition[T,U,V] = {
    Transition[T,U,V](() =>pipeline.filter(guard(ctx)).map(_ => next).map(o=>{
      //      on_transition(o.nested)
      o
    }), current)
  }

  def onTransition(f: T => Unit):Builder[T,U,V] = {
    on_transition = f
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

