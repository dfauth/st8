package st8

case class State[T,U,V](state:T) {
  val map = scala.collection.mutable.Map[V, Transition[T,U,V]]()
  def trigger(e: V, ctx:U):State[T,U,V] = {
    map.get(e).map(t => t.execute(ctx)).getOrElse(this)
  }
  def onEntry(f:Function[T, Unit]):State[T,U,V] = {this}
  def onExit(f:Function[T, Unit]):State[T,U,V] = {this}
  def onEvent(e:V):Transition[T,U,V] = {
    val t = Transition[T,U,V]()
    map(e) = t
    t
  }
  def nested:T = state
}
