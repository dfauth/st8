package st8

import akka.actor.FSM

/**
  *        A
  *      /  \
  *    A1   A2
  *   /      \
  *  B---B1---C
  *   \      /
  *    B2  C1
  *     \ /
  *      D
  */
class St8ctor extends FSM[St8, D8a] {

  startWith(A, Data)
  when(A) {
    case Event(A1(), Data) => goto(B)
    case Event(A2(), Data) => goto(C)
  }
  when(B) {
    case Event(B1(), Data) => goto(C)
    case Event(B2(), Data) => goto(D)
  }
  when(C) {
    case Event(C1(), Data) => goto(D)
  }

  onTransition {
    case (a,b) => sender() ! CurrentState(a,b)

  }
}

sealed trait St8

case object A extends St8
case object B extends St8
case object C extends St8
case object D extends St8

sealed trait D8a

case object Data extends D8a

// messages
case class A1()
case class A2()
case class B1()
case class B2()
case class C1()
case class CurrentState(a:Any, b:Any)