package state

import com.typesafe.scalalogging.LazyLogging
import org.scalatest._

class StateMonadSpec extends FlatSpec with Matchers with LazyLogging {

  case class StateMonad[S, A](run: S => (A, S)) {

    def map[B](f: A => B): StateMonad[S, B] =
      StateMonad(s => {
        val (a, t) = run(s)
        (f(a), t)
      })

    def flatMap[B](f: A => StateMonad[S, B]): StateMonad[S, B] =
      StateMonad(s => {
        val (a, t) = run(s)
        f(a) run t
      })

    def eval(s: S): A =
      run(s)._1
  }

  object StateMonad {

    def insert[S, A](a: A): StateMonad[S, A] =
      StateMonad(s => (a, s))

    def get[S, A](f: S => A): StateMonad[S, A] =
      StateMonad(s => (f(s), s))

    def mod[S](f: S => S): StateMonad[S, Unit] =
      StateMonad(s => ((), f(s)))
  }

  type Transition = MyState => Event => MyState
  val NO_OP:Transition = s => e => s

  // states
  sealed trait MyState {

    def onEvent(e:Event):Transition = trigger(e){case _ => NO_OP}

    def trigger(e:Event)(pf:PartialFunction[Event, Transition]):Transition = {
      if(pf.isDefinedAt(e)) {
        pf(e)
      } else {
        NO_OP  // default behaviour: do nothing
      }
    }
  }

  case object A extends MyState {
    override def onEvent(e:Event) = trigger(e) {
      case A1 => (s:MyState) => (e:Event) => B
      case A2 => (s:MyState) => (e:Event) => C
    }
  }

  case object B extends MyState {
    override def onEvent(e:Event) = trigger(e) {
      case B1 => (s:MyState) => (e:Event) => C
      case B2 => (s:MyState) => (e:Event) => D
    }
  }

  case object C extends MyState {
    override def onEvent(e:Event) = trigger(e) {
      case C1 => (s:MyState) => (e:Event) => D
    }
  }

  case object D extends MyState


  // events
  sealed trait Event
  case object A2 extends Event
  case object A1 extends Event
  case object B1 extends Event
  case object B2 extends Event
  case object C1 extends Event

  // side effects
  sealed trait SideEffect
  case object SE1 extends SideEffect

  // state container
  sealed trait State {
    val current:MyState
  }
  case object initial extends State {
    override val current: MyState = A
  }

//  case class TransitionBuilder[S,A](parent:StateMachineBuilder[S,A], from: S) {
//
//    def onEvent(e: A):TransitionBuilder[S,A] = ???
//
//    def goTo(to: S):StateMachineBuilder[S,A] = {
//      parent.accept(build(from => to))
//    }
//
//    def build(f: S => S):Transition[S,A] = ??? //current => current match {
////      case s => (SE1, f(s))
////    }
//
//  }
//
//  case class StateMachineBuilder[S,A]() {
//    def accept(build: Transition[S,A]):StateMachineBuilder[S,A] = {
//      this
//    }
//
//    def forState(s: S):TransitionBuilder[S,A] = TransitionBuilder[S,A](this, s)
//
//    def build:StateMachine[S,A] = ???
//  }
//
//  case class StateMachine[S,A]() {
//    def trigger(e:Event):Transition[S,A] = ???
//  }
//
//  val builder = StateMachineBuilder[MyState, MyEvent]()
//  builder.forState(A).onEvent(A1).goTo(B).
//    forState(A).onEvent(A2).goTo(C).
//    forState(B).onEvent(B1).goTo(C).
//    forState(B).onEvent(B2).goTo(D).
//    forState(C).onEvent(C1).goTo(D)
//
//  val sm = builder.build

  "A State" should "be able to handle this" in {
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
      {
        // initial state
        val sm = StateMonad.insert[State, MyState](A)

        sm.eval(initial) should be (A)

        // current state
        val sc = StateMonad[State, MyState](c => (A, c))
        val current = sc.eval(initial)
        current should be (A)

        val transition = current.onEvent(A1)
        logger.info(s"transition(current): ${transition(current)}")
        logger.info(s"transition(current)(A1): ${transition(current)(A1)}")

        val x:MyState => MyState = transition(_)(A1)

        val t:MyState => StateMonad[State,MyState] = ms => StateMonad[State, MyState](s => (x(sm.eval(s)), s))
        val next = sm.flatMap(t)
        logger.info(s"next: ${next}")
        next.eval(initial) should be (B)
//        sc.flatMap(transition).eval(initial)
//        State(transition).eval(A) should be (B)
      }
  }
}