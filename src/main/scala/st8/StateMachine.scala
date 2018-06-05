package st8


object StateMachine {
  def create[T,U,V](str: String):Builder[T,U,V] = {
    Builder[T,U,V]()
  }

}

case class StateMachine[T,U,V](initial:Option[State[T,U,V]]) {

  var current: State[T,U,V] = initial.get

  def currentState():State[T,U,V] = current

  def trigger(e: V, ctx:U):StateMachine[T,U,V] = {
    current.trigger(e, ctx)
    this
  }
}

case class Builder[T,U,V]() {

  var initial: Option[State[T,U,V]] = Option.empty

  def build():StateMachine[T,U,V] = {
    StateMachine(initial)
  }

  def state(): Any = {}

  def initialState(state:T):State[T,U,V] = {
    initial = Option(State[T,U,V](state))
    initial.get
  }

  def state(state:T):State[T,U,V] = State[T,U,V](state)
}
