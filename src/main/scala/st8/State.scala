package st8

case class State[T,U,V](ctx:U, nested:T, transitions:Iterable[Transition[T,U,V]]) {
  var map:Map[V, Transition[T,U,V]] = transitions.foldLeft(Map.empty[V, Transition[T,U,V]]){
    (m:Map[V, Transition[T,U,V]],t:Transition[T,U,V])=>{
      m ++ Map[V, Transition[T,U,V]](t.event->t)
    }
  }
  def trigger(e: V):State[T,U,V] = {
    map.get(e).map(t => t.execute()).getOrElse(this)
  }
}

case class StateBuilder[T,U,V](ctx:U, nested:T, parent:StateMachineBuilder[T,U,V]) {

  var current:State[T,U,V] = State(ctx, nested, Iterable.empty[Transition[T,U,V]])
  val map = scala.collection.mutable.Map[V, TransitionBuilder[T,U,V]]()

  def state(): State[T, U, V] = state(nested)
  def state(t: T): State[T, U, V] = {state(t, Iterable.empty)}
  def state(t: T, i: Iterable[Transition[T,U,V]]): State[T, U, V] = {
    if(t == nested) {
      current.map ++= i.foldLeft(Map.empty[V, Transition[T,U,V]]){(m,t)=>Map(t.event->t)}
      current
    } else {
      parent.state(t).state(t)
    }
  }

  def build():State[T,U,V] = {
    state(nested, map.values.map(_.build()))
  }

  def onEntry(f:Function[Transition[T,U,V], Unit]):StateBuilder[T,U,V] = {this}
  def onExit(f:Function[Transition[T,U,V], Unit]):StateBuilder[T,U,V] = {this}
  def onEvent(e:V):TransitionBuilder[T,U,V] = {
    val t = Transition.create[T,U,V](e, ctx, this)
    map(e) = t
    t
  }
}
