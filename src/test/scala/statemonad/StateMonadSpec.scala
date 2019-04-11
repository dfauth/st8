package state

import com.typesafe.scalalogging.LazyLogging
import org.scalatest._

class StateMonad1Spec extends FlatSpec with Matchers with LazyLogging {

  case class State[S, A](run: S => (A, S)) {

    def map[B](f: A => B): State[S, B] =
      State(s => {
        val (a, t) = run(s)
        (f(a), t)
      })

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
        val (a, t) = run(s)
        f(a) run t
      })

    def eval(s: S): A =
      run(s)._1
  }

  object State {

    def insert[S, A](a: A): State[S, A] =
      State(s => (a, s))

    def get[S, A](f: S => A): State[S, A] =
      State(s => (f(s), s))

    def mod[S](f: S => S): State[S, Unit] =
      State(s => ((), f(s)))
  }

  trait Colour
  case object Red extends Colour
  case object Yellow extends Colour
  case object Green extends Colour

  trait LightState
  case object RedState extends LightState
  case object YellowStateClimbing extends LightState
  case object YellowStateFalling extends LightState
  case object GreenState extends LightState
  
  val transitions:LightState => (Colour, LightState) = l => l match {
    case RedState =>
      (Yellow, YellowStateFalling)
    case YellowStateFalling =>
      (Green, GreenState)
    case GreenState =>
      (Yellow, YellowStateClimbing)
    case YellowStateClimbing =>
      (Red, RedState)
  }

  "A State" should "be able to handle this" in {
    /**
      *        R      R
      *         \    /
      *         Y   Y
      *          \ /
      *           G
      */
      {
        State.insert(Red).
          eval(RedState) should be (Red) // starting from Red, after 0 transitions, should be Yellow
      }
      {
        State[LightState, Colour](transitions).
          eval(RedState) should be (Yellow) // starting from Red, after 1 transitions, should be Yellow
      }
      {
        State[LightState, Colour](transitions).
          flatMap(a => State[LightState, Colour](transitions)).
          eval(RedState) should be (Green) // starting from Red, after 2 transitions, should be Green
      }
      {
        State[LightState, Colour](transitions).
          flatMap(a => State[LightState, Colour](transitions)).
          flatMap(a => State[LightState, Colour](transitions)).
          eval(RedState) should be (Yellow) // starting from Red, after 3 transitions, should be Yellow
      }

    {
      State[LightState, Colour](transitions).
      flatMap(a => State[LightState, Colour](transitions)).
      flatMap(a => State[LightState, Colour](transitions)).
      flatMap(a => State[LightState, Colour](transitions)).
      eval(RedState) should be (Red) // starting from Red, after 4 transitions, should be Red
    }
  }
}