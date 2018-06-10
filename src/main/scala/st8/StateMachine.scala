package st8


object StateMachine {
  def create[T,U,V](name: String, ctx:U):StateMachineBuilder[T,U,V] = {
    StateMachineBuilder[T,U,V](ctx)
  }

}

case class StateMachine[T,U,V](initial:State[T,U,V]) {

  var current: State[T,U,V] = initial

  def currentState():State[T,U,V] = current

  def trigger(e: V):StateMachine[T,U,V] = {
    current.trigger(e)
    this
  }
}

case class StateMachineBuilder[T,U,V](ctx:U) {

  var initial: State[T,U,V] = _
  var map:Map[T,State[T,U,V]] = Map()

  def build():StateMachine[T,U,V] = {
    StateMachine(initial)
  }

  def state(): Any = {}

  def initialState(state:T):State[T,U,V] = {
    initial = this.state(state)
    initial
  }

  def state(state:T):State[T,U,V] = {
    map.get(state).getOrElse({
      val s = State[T,U,V](ctx, state)
      map =  map ++ Map(state -> s)
      s
    })
  }
}

