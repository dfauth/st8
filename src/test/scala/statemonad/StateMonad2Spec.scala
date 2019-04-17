package statemonad

import com.typesafe.scalalogging.LazyLogging
import org.scalatest._

class StateMonad2Spec extends FlatSpec with Matchers with LazyLogging {

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

  type Transition = StateMonad[State, MyState]

  // states
  sealed trait MyState
  case object A extends MyState
  case object B extends MyState
  case object C extends MyState
  case object D extends MyState

  case class State(current:MyState)

  object Initial extends State(A)

  // side effects
  sealed trait SideEffect
  case object SE1 extends SideEffect

  def toStateMonad(pf: PartialFunction[MyState,MyState]):Transition = {
    StateMonad[State, MyState](s => {
      val b = if(pf.isDefinedAt(s.current)) {
        pf(s.current)
      } else {
        s.current
      }
      (b, State(b))
    })
  }

  // events
  def A1: Transition = toStateMonad {
    case A => B
  }

  def A2: Transition = toStateMonad {
    case A => C
  }

  def B1: Transition = toStateMonad {
    case B => C
  }

  def B2: Transition = toStateMonad {
    case B => D
  }

  def C1: Transition = toStateMonad {
    case C => D
  }

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

        // current state
        val sc = StateMonad[State, MyState](c => (c.current, c))
        val current = sc.eval(Initial)
        current should be (A)

        {
          val next = for {
            next <- A1
          } yield next
            next.eval(Initial) should be (B)
        }

        {
          var next = for {
            next <- A1
            next <- B1
          } yield next
          next.eval(Initial) should be (C)
        }

        {
          var next = for {
            next <- A1
            next <- B1
            next <- C1
          } yield next
          next.eval(Initial) should be (D)
        }

        {
          var next = for {
            next <- A1
            next <- B2
          } yield next
          next.eval(Initial) should be (D)
        }

        {
          var next = for {
            next <- A2
            next <- A1
          } yield next
          next.eval(Initial) should be (C)
        }

        {
          var next = for {
            next <- A2
            next <- C1
          } yield next
          next.eval(Initial) should be (D)
        }

        {
          var next = for {
            next <- A2
            next <- C1
            next <- A1
          } yield next
          next.eval(Initial) should be (D)
        }

        {
          val events:Seq[Transition] = Seq[Transition](A2,C1,A1)

          events.reduce((t1, t2) => {
            for {
              a <- t1
              b <- t2
            } yield b
          }).eval(Initial) should be (D)

        }
      }
  }
}