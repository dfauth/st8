package st8


object StateMachine {
  def create[T,U,V](name: String, ctx:U):StateMachineBuilder[T,U,V] = {
    StateMachineBuilder[T,U,V](ctx)
  }

}

case class StateMachine[T,U,V](initial:State[T,U,V], states:Iterable[State[T,U,V]]) {

  var map:Map[T,State[T,U,V]] = states.foldLeft[Map[T,State[T,U,V]]](Map.empty[T, State[T,U,V]]){
    (m, s) => m ++ Map(s.nested -> s)
  }
  var current: State[T,U,V] = initial

  def currentState():State[T,U,V] = current

  def trigger(e: V):StateMachine[T,U,V] = {
    current = current.trigger(e)
    this
  }
}

case class StateMachineBuilder[T,U,V](ctx:U) {

  var initial: StateBuilder[T,U,V] = _
  var map:Map[T,StateBuilder[T,U,V]] = Map()

  def build():StateMachine[T,U,V] = {
    StateMachine(initial.build(), map.values.map(_.build()).seq)
  }

  def state(): Any = {}

  def initialState(state:T):StateBuilder[T,U,V] = {
    initial = this.state(state)
    initial
  }

  def state(state:T):StateBuilder[T,U,V] = {
    map.get(state).getOrElse({
      val s = StateBuilder[T,U,V](ctx, state, this)
      map =  map ++ Map(state -> s)
      s
    })
  }
}

