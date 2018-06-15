import org.apache.logging.log4j.scala.Logging
import org.scalatest._
import st8.{StateMachine, Transition}

class StateMachineSpec extends FlatSpec with Matchers with Logging {

  var shouldProceed = false
  var wasCalled = scala.collection.mutable.Map[String, Boolean]()

  def predicate(ctx:StateMachineSpec):MyEvent=>Boolean = {
    event => ctx.shouldProceed
  }

  def callback(message: String):Transition[MyState,StateMachineSpec,MyEvent] => Unit = { t => {
    logger.info(message+" transition: "+t);
    val m = wasCalled.get(message).getOrElse({
      wasCalled(message) = true
      true
    })
  }}
  def assertOnlyTheseCallbacksWereCalledAndReset(messages:String*):Unit = {
    assertCallbacksWereCalledAndResetSeq(messages)
    assertNoCallbacksWereCalled()
  }
  def assertCallbacksWereCalledAndReset(messages:String*):Unit = {
    assertCallbacksWereCalledAndResetSeq(messages)
  }
  def assertCallbacksWereCalledAndResetSeq(messages:Seq[String]):Unit = {
    messages.foreach(message => {
      wasCalled.get(message).getOrElse(false) should be (true)
      wasCalled(message) = false
    })
  }
  def assertCallbackWasNotCalled(message:String):Unit = {
    wasCalled.get(message).isEmpty should be (true)
  }

  val f = new PartialFunction[Boolean, Boolean] {
    def apply(b: Boolean) = {
      b == true
    }
    def isDefinedAt(b: Boolean) = b == true
  }

  def assertNoCallbacksWereCalled():Unit = {
    wasCalled.values.collectFirst[Boolean](f).isEmpty should be (true)
  }

  def resetAllCallbacks():Unit = {
    wasCalled.clear()
  }


  "A State" should "do something" in {
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
    val builder = StateMachine.create[MyState,StateMachineSpec,MyEvent]("name", this)
    builder should not be (None)

    builder initialState A onEntry callback("onEntryA") onExit callback("onExitA")
    builder state(B) onEntry(callback("onEntryB")) onExit(callback("onExitB"))
    builder state(C) onEntry(callback("onEntryC")) onExit(callback("onExitC"))
    builder state(D) onEntry(callback("onEntryD")) onExit(callback("onExitD"))
    builder state(A) onEvent(A1) unless(predicate) goTo(B) onTransition(callback("onTransitionA1"))
    builder state(A) onEvent(A2) unless(predicate) goTo(C) onTransition(callback("onTransitionA2"))
    builder state(B) onEvent(B1) unless(predicate) goTo(C) onTransition(callback("onTransitionB1"))
    builder state(B) onEvent(B2) unless(predicate) goTo(D) onTransition(callback("onTransitionB2"))
    builder state(C) onEvent(C1) unless(predicate) goTo(D) onTransition(callback("onTransitionC1"))

    {
      val stateMachine = builder.build()
      stateMachine should not be (None)
      stateMachine.currentState.nested should be (A)
      assertNoCallbacksWereCalled()
      stateMachine trigger(A1)
      stateMachine.currentState.nested should be (A)
      assertNoCallbacksWereCalled()
      shouldProceed = true
      stateMachine trigger(A1)
      stateMachine.currentState.nested should be (B)
      assertOnlyTheseCallbacksWereCalledAndReset("onTransitionA1","onExitA","onEntryB")
      stateMachine trigger(B1)
      stateMachine.currentState.nested should be (C)
      assertOnlyTheseCallbacksWereCalledAndReset("onTransitionB1","onExitB","onEntryC")
      stateMachine trigger(C1)
      stateMachine.currentState.nested should be (D)
      assertOnlyTheseCallbacksWereCalledAndReset("onTransitionC1","onExitC","onEntryD")
      resetAllCallbacks()
      stateMachine trigger(C1)
      stateMachine.currentState.nested should be (D)
      assertNoCallbacksWereCalled()
    }
    {
      shouldProceed = false
      val stateMachine = builder.build()
      stateMachine should not be (None)
      stateMachine.currentState.nested should be (A)
      assertNoCallbacksWereCalled()
      stateMachine trigger(A2)
      stateMachine.currentState.nested should be (A)
      assertNoCallbacksWereCalled()
      shouldProceed = true
      stateMachine trigger(A2)
      stateMachine.currentState.nested should be (C)
      assertOnlyTheseCallbacksWereCalledAndReset("onTransitionA2","onExitA","onEntryC")
      stateMachine trigger(B1)
      stateMachine.currentState.nested should be (C)
      assertNoCallbacksWereCalled()
      stateMachine trigger(C1)
      stateMachine.currentState.nested should be (D)
      assertOnlyTheseCallbacksWereCalledAndReset("onTransitionC1","onExitC","onEntryD")
      resetAllCallbacks()
      stateMachine trigger(B1)
      stateMachine.currentState.nested should be (D)
      assertNoCallbacksWereCalled()
    }
  }

  // states
  object A extends MyState("A") {
  }
  object B extends MyState("B") {}
  object C extends MyState("C") {}
  object D extends MyState("D") {}

  case class MyState(id:String){}
  case class MyEvent(id:String){}

  // events
  object A1 extends MyEvent("A1") {}
  object A2 extends MyEvent("A2") {}
  object B1 extends MyEvent("B1") {}
  object B2 extends MyEvent("B2") {}
  object C1 extends MyEvent("C1") {}

}