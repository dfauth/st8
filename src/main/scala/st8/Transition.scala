package st8

case class Transition[T, U, V]() {

  var next:State[T,U,V] = _
  var guard:U=>Boolean = u=>true
  val pipeline:Option[U] = Option.empty

  def execute(ctx:U): State[T,U,V] = {
    // TODO check the guard
    // TODO on transition
    next
  }

  def onTransition(f: T => Unit):Transition[T,U,V] = {this}

  def goTo(state:T):Transition[T,U,V] = {
    next = State[T,U,V](state)
    this
  }

  def unless(p: U => Boolean):Transition[T,U,V] = {
    pipeline.filter(p)
    this
  }
}
