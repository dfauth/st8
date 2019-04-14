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

  // events
  sealed trait Event
  case object A2 extends Event
  case object A1 extends Event
  case object B1 extends Event
  case object B2 extends Event
  case object C1 extends Event

  type Transition = PartialFunction[(Event, MyState), MyState]
  val NO_OP:Transition = {case (e,s) => s}

  // states
  sealed trait MyState
  case object A extends MyState
  case object B extends MyState
  case object C extends MyState
  case object D extends MyState

  case class State(current:MyState) {

    val transitions:Transition = {
      case (A1, A) => B
      case (A2, A) => C
      case (B1, B) => C
      case (B2, B) => D
      case (C1, C) => D
    }
  }

  object Initial extends State(A)

  // side effects
  sealed trait SideEffect
  case object SE1 extends SideEffect

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

        sm.eval(Initial) should be (A)

        val t:Event => MyState => StateMonad[State,MyState] = e => ms => StateMonad[State, MyState](s => {
          val result = s.transitions.orElse(NO_OP)(e,s.current)
          (result, State(result))
        })

        // current state
        val sc = StateMonad[State, MyState](c => (c.current, c))
        val current = sc.eval(Initial)
        current should be (A)

        {
          val next = sm.flatMap(t(A1))
          next.eval(Initial) should be (B)
        }

        {
          var next = sm.flatMap(t(A1))
            next = next.flatMap(t(B1))
          next.eval(Initial) should be (C)
        }

        {
          var next = sm.flatMap(t(A1))
            next = next.flatMap(t(B1))
            next = next.flatMap(t(C1))
          next.eval(Initial) should be (D)
        }

        {
          var next = sm.flatMap(t(A1))
          next = next.flatMap(t(B2))
          next.eval(Initial) should be (D)
        }

        {
          var next = sm.flatMap(t(A2))
          next.eval(Initial) should be (C)
          next = next.flatMap(t(A1))  // invalid transitions have no effect
          next.eval(Initial) should be (C)
        }

        {
          var next = sm.flatMap(t(A2))
            next = next.flatMap(t(C1))
          next.eval(Initial) should be (D)
        }

        {
          var next = sm.flatMap(t(A2))
            next = next.flatMap(t(C1))
            next = next.flatMap(t(A1))  // invalid transitions have no effect
          next.eval(Initial) should be (D)
        }
      }
  }
}