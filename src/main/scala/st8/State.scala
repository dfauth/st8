package st8

case class State[T,U,V](ctx:U,
                        nested:T,
                        transitions:Iterable[Transition[T,U,V]] = Iterable.empty[Transition[T,U,V]],
                        var stateChangeListeners:List[StateChangeListener[T,U,V]] = List.empty[StateChangeListener[T,U,V]]
                       ) {

  var map:Map[V, Transition[T,U,V]] = transitions.foldLeft(Map.empty[V, Transition[T,U,V]]){
    (m:Map[V, Transition[T,U,V]],t:Transition[T,U,V])=>{
      m ++ Map[V, Transition[T,U,V]](t.event->t)
    }
  }

  def trigger(e: V):State[T,U,V] = {
    map.get(e).map(t => t.execute(e)).getOrElse(this)
  }
  def entry(t:Transition[T,U,V]):Unit = stateChangeListeners.foreach(_.onEntry(t))
  def exit(t:Transition[T,U,V]):Unit = stateChangeListeners.foreach(_.onExit(t))
}

trait StateChangeListener[T,U,V] {
  def onEntry(t:Transition[T,U,V]):Unit = {}
  def onExit(t:Transition[T,U,V]):Unit = {}
}

case class EntryStateChangeListener[T,U,V](f:Transition[T,U,V]=>Unit) extends StateChangeListener[T,U,V] {
  override def onEntry(t:Transition[T,U,V]):Unit = f(t)
}

case class ExitStateChangeListener[T,U,V](f:Transition[T,U,V]=>Unit) extends StateChangeListener[T,U,V] {
  override def onExit(t:Transition[T,U,V]):Unit = f(t)
}

case class StateBuilder[T,U,V](ctx:U, nested:T, parent:StateMachineBuilder[T,U,V]) {

  var current:State[T,U,V] = State(ctx, nested)
  val map = scala.collection.mutable.Map[V, TransitionBuilder[T,U,V]]()
  var stateChangeListeners:List[StateChangeListener[T,U,V]] = List.empty[StateChangeListener[T,U,V]]

  def state(t: T = nested,
            i: Iterable[Transition[T,U,V]] = Iterable.empty,
            l:List[StateChangeListener[T,U,V]] = stateChangeListeners
           ): State[T, U, V] = {
    if(t == nested) {
      current.map ++= i.foldLeft(Map.empty[V, Transition[T,U,V]]){(m,t)=>m ++ Map(t.event->t)}
      current.stateChangeListeners = current.stateChangeListeners ::: l
      current
    } else {
      parent.state(t).state(t)
    }
  }

  def build():State[T,U,V] = {
    state(nested, map.values.map(_.build()), stateChangeListeners)
  }

  def onEntry(f:Transition[T,U,V] => Unit):StateBuilder[T,U,V] = {
    stateChangeListeners = stateChangeListeners :+ EntryStateChangeListener(f)
    this
  }
  def onExit(f:Transition[T,U,V] => Unit):StateBuilder[T,U,V] = {
    stateChangeListeners = stateChangeListeners :+ ExitStateChangeListener(f)
    this
  }
  def onEvent(e:V):TransitionBuilder[T,U,V] = {
    val t = Transition.create[T,U,V](e, ctx, this)
    map(e) = t
    t
  }
}
