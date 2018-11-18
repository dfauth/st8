package st8

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ActorSpec() extends TestKit(ActorSystem("st8Spec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "An FSM actor" must {

    "blah blah" in {
      val st8ctor = system.actorOf(Props(new St8ctor))
      st8ctor ! A1
    }

  }
}

