import org.apache.logging.log4j.scala.Logging
import org.scalatest._
import st8.{State, StateMachine,Transition}

class StateMachineSpec extends FlatSpec with Matchers with Logging {

  var shouldProceed = false
  var wasCalled = scala.collection.mutable.Map[String, Boolean]()
  def predicate(ctx:StateMachineSpec): State[MyState,StateMachineSpec,MyEvent] => Boolean = {a => shouldProceed}
  def callback(message: String):Transition[MyState,StateMachineSpec,MyEvent] => Unit = { t => {
    logger.info(message+" transition: "+t);
    val m = wasCalled.get(message).getOrElse({
      wasCalled(message) = true
      true
    })
  }}
  def assertCallbackWasCalledAndReset(message:String):Unit = {
    wasCalled(message) should be (true)
    wasCalled(message) = false
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

    builder initialState A onEntry callback("onEntryA") onExit callback("onExitB")
    builder.state(B).onEntry(callback("onEntryA")).onExit(callback("onExitB"))
    builder.state(A).onEvent(A1).unless(predicate).goTo(B).onTransition(callback("onTransitionA1"))
    builder.state(A).onEvent(A2).unless(predicate).goTo(C).onTransition(callback("onTransitionA2"))
    builder.state(B).onEvent(B1).unless(predicate).goTo(C).onTransition(callback("onTransitionB1"))
    builder.state(B).onEvent(B2).unless(predicate).goTo(D).onTransition(callback("onTransitionB2"))
    builder.state(C).onEvent(C1).unless(predicate).goTo(D).onTransition(callback("onTransitionC1"))

    //
//    builder.initial(A)
//      .onEntry(loggingConsumer("onEntry"))
//      .onExit(loggingConsumer("onExit"))
//      .state(B)
//      .onEntry(loggingConsumer("onEntry"))
//      .onExit(loggingConsumer("onExit"))
//      .state(C)
//      .onEntry(loggingConsumer("onEntry"))
//      .onExit(loggingConsumer("onExit"))
//      .state(D)
//      .onEntry(loggingConsumer("onEntry"))
//      .onExit(loggingConsumer("onExit"))
//      .from(A)
//      .onEvent(A1)
//      .unless(predicate())
//      .goTo(B)
//      .onTransition(loggingConsumer("onTransition"))
//      .then()
//      .from(A)
//      .onEvent(A2)
//      .unless(predicate())
//      .goTo(C)
//      .onTransition(loggingConsumer("onTransition"))
//      .then()
//      .from(B)
//      .onEvent(B1)
//      .unless(predicate())
//      .goTo(C)
//      .onTransition(loggingConsumer("onTransition"))
//      .and()
//      .from(A)
//      .onEvent(B2)
//      .unless(predicate())
//      .goTo(D)
//      .onTransition(loggingConsumer("onTransition"))
//      .then()
//      .from(C)
//      .onEvent(C1)
//      .unless(predicate())
//      .goTo(D)
//      .onTransition(loggingConsumer("onTransition"))
//    ;
//
//    {
//      StateMachine stateMachine = builder.build();
//      Assert.assertEquals(stateMachine.current(), A);
//      Assert.assertEquals(stateMachine.trigger(A1).current(), A);
//    }
    val stateMachine = builder.build()
    stateMachine should not be (None)
    stateMachine.currentState.nested should be (A)
    stateMachine trigger(A1)
    stateMachine.currentState.nested should be (A)
    shouldProceed = true
    stateMachine trigger(A1)
    stateMachine.currentState.nested should be (B)
    assertCallbackWasCalledAndReset("onTransitionA1")
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