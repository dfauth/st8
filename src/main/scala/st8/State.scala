package st8

case class State[T,U,V](ctx:U, nested:T) {
  val map = scala.collection.mutable.Map[V, Builder[T,U,V]]()
  def trigger(e: V):State[T,U,V] = {
    map.get(e).map(t => t.build().execute()).getOrElse(this)
  }
  def onEntry(f:Function[Transition[T,U,V], Unit]):State[T,U,V] = {this}
  def onExit(f:Function[Transition[T,U,V], Unit]):State[T,U,V] = {this}
  def onEvent(e:V):Builder[T,U,V] = {
    val t = Transition.create[T,U,V](ctx, this)
    map(e) = t
    t
  }
}
