package statemonad

import akka.Done
import com.typesafe.scalalogging.LazyLogging
import org.scalatest._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class StateMonad2Spec extends FlatSpec with Matchers with LazyLogging {

  def toggle: Boolean = false


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

  type Result = (MyState, Set[SideEffect])
  type Transition = StateMonad[State, Result]

  // states
  case class MyState(
                 onEntry:Set[SideEffect] = NONE,
                 onExit:Set[SideEffect] = NONE
               ) {

  }

  object A extends MyState(onExit = Set(A_ON_EXIT_SIDE_EFFECT))
  object B extends MyState
  object C extends MyState(onExit = Set(C_ON_EXIT_SIDE_EFFECT))
  object D extends MyState(Set(D_ON_ENTRY_SIDE_EFFECT))

  case class State(current:MyState)

  object Initial extends State(A)

  // side effects
  sealed trait SideEffect

  trait ExecutableSideEffect[C] extends SideEffect {

    protected def _execute(ctx: C): Done

    final def execute(ctx:C):Future[Done] = Future { _execute(ctx) }
  }

  trait NOOP[C] extends ExecutableSideEffect[C] {
    override def _execute(ctx: C): Done = Done
  }

  case object A_ON_EXIT_SIDE_EFFECT extends SideEffect with NOOP[StateMonad2Spec]
  case object C_ON_EXIT_SIDE_EFFECT extends NOOP[StateMonad2Spec]
  case object D_ON_ENTRY_SIDE_EFFECT extends ExecutableSideEffect[StateMonad2Spec] {
    override def _execute(ctx: StateMonad2Spec): Done = {
      logger.info(s" executing ${this} side effect in context: ${ctx}")
      Done
    }
  }
  case object C_TO_D_TRANSITION_SIDE_EFFECT extends NOOP[StateMonad2Spec]
  case object B_TO_C_TRANSITION_SIDE_EFFECT extends NOOP[StateMonad2Spec]

  def toStateMonad(pf: PartialFunction[MyState,Result]):Transition = {
    StateMonad[State, Result](s => {
      val (b, sideEffects) = if(pf.isDefinedAt(s.current)) {
        pf(s.current)
      } else {
        (s.current, NONE.toSet)
      }
      ((b, sideEffects), State(b))
    })
  }

  val NONE: Set[SideEffect] = Set.empty[SideEffect]

  def transition(t: (MyState, MyState), sideEffects: SideEffect*): Result = {
    val (from, to) = t
    (to, from.onExit ++ sideEffects ++ to.onEntry )
  }

  object TEST_GUARD extends (StateMonad2Spec => Boolean) {
    override def apply(v1: StateMonad2Spec): Boolean = v1.toggle
  }

  // events
  def A1(ctx:StateMonad2Spec): Transition = toStateMonad {
    case A if(!TEST_GUARD(ctx)) => (B, NONE)
  }

  def A2: Transition = toStateMonad {
    case A => transition(A -> C)
  }

  def B1: Transition = toStateMonad {
    case B => transition(B -> C, B_TO_C_TRANSITION_SIDE_EFFECT)
  }

  def B2: Transition = toStateMonad {
    case B => transition(B -> D)
  }

  def C1: Transition = toStateMonad {
    case C => transition(C -> D, C_TO_D_TRANSITION_SIDE_EFFECT)
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
            next <- A1(StateMonad2Spec.this)
          } yield next
            next.eval(Initial)._1 should be (B)
        }

        {
          var next = for {
            next <- A1(StateMonad2Spec.this)
            next <- B1
          } yield next
          next.eval(Initial)._1 should be (C)
        }

        {
          var next = for {
            next <- A1(StateMonad2Spec.this)
            next <- B1
            next <- C1
          } yield next
          next.eval(Initial)._1 should be (D)
        }

        {
          var next = for {
            next <- A1(StateMonad2Spec.this)
            next <- B2
          } yield next
          next.eval(Initial)._1 should be (D)
        }

        {
          var next = for {
            next <- A2
            next <- A1(StateMonad2Spec.this)
          } yield next
          next.eval(Initial)._1 should be (C)
        }

        {
          var next = for {
            next <- A2
            next <- C1
          } yield next
          next.eval(Initial)._1 should be (D)
        }

        {
          var next = for {
            next <- A2
            next <- C1
            next <- A1(StateMonad2Spec.this)
          } yield next
          next.eval(Initial)._1 should be (D)
        }

        {
          val events:Seq[Transition] = Seq[Transition](A2,C1,A1(StateMonad2Spec.this))

          events.reduce((t1, t2) => {
            for {
              a <- t1
              b <- t2
            } yield b
          }).eval(Initial)._1 should be (D)
        }

        {
          // restore state from event history
          val events:Seq[Transition] = Seq[Transition](A1(StateMonad2Spec.this),B1)

          val sm = events.reduce((t1, t2) => {
            for {
              a <- t1
              b <- t2
            } yield b
          })
          sm.eval(Initial)._1 should be (C)

          // apply a new event
          val e:Transition = C1

          val result = (for {
            s <- sm
            x <- e
          } yield x).eval(Initial)
          result._1 should be (D)
          result._2 should be (Set(C_ON_EXIT_SIDE_EFFECT, C_TO_D_TRANSITION_SIDE_EFFECT, D_ON_ENTRY_SIDE_EFFECT))
          result._2.foreach {
            case e: ExecutableSideEffect[StateMonad2Spec] => {
              e.execute(StateMonad2Spec.this)
            }
          }
        }
        Thread.sleep(2 * 1000)
      }
  }
}

