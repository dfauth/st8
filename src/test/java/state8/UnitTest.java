package state8;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.function.Consumer;
import java.util.function.Predicate;

import static state8.UnitTest.Event.*;
import static state8.UnitTest.State.*;

public class UnitTest {

    private static final Logger logger = LoggerFactory.getLogger(UnitTest.class);
    private boolean[] bool = new boolean[] {false};

    @Test
    public void testIt() {
        try {
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
            StateMachine.Builder builder = StateMachine.create("name", this);
            Assert.assertNotNull(builder);

            builder.initial(A)
                        .onEntry(loggingConsumer("onEntry"))
                        .onExit(loggingConsumer("onExit"))
                    .state(B)
                        .onEntry(loggingConsumer("onEntry"))
                        .onExit(loggingConsumer("onExit"))
                    .state(C)
                        .onEntry(loggingConsumer("onEntry"))
                        .onExit(loggingConsumer("onExit"))
                    .state(D)
                        .onEntry(loggingConsumer("onEntry"))
                        .onExit(loggingConsumer("onExit"))
                    .from(A)
                        .onEvent(A1)
                        .unless(predicate())
                        .goTo(B)
                        .onTransition(loggingConsumer("onTransition"))
                    .then()
                    .from(A)
                        .onEvent(A2)
                        .unless(predicate())
                        .goTo(C)
                        .onTransition(loggingConsumer("onTransition"))
                    .then()
                    .from(B)
                        .onEvent(B1)
                        .unless(predicate())
                        .goTo(C)
                        .onTransition(loggingConsumer("onTransition"))
                    .and()
                    .from(A)
                        .onEvent(B2)
                        .unless(predicate())
                        .goTo(D)
                        .onTransition(loggingConsumer("onTransition"))
                    .then()
                        .from(C)
                        .onEvent(C1)
                        .unless(predicate())
                        .goTo(D)
                        .onTransition(loggingConsumer("onTransition"))
                    ;

            {
                StateMachine stateMachine = builder.build();
                Assert.assertEquals(stateMachine.current(), A);
                Assert.assertEquals(stateMachine.trigger(A1).current(), A);
            }

//            bool[0] = true;
//            Assert.assertEquals(stateMachine.trigger(A1).current(), B);
//
//            boolean[] wasCalled = new boolean[] {false};
//            builder.state("first").onEntry(t -> {
//                wasCalled[0] = true;
//            });
//            stateMachine = builder.build();
//            Assert.assertEquals(stateMachine.trigger(1).current(), "first");
//            Assert.assertTrue(wasCalled[0]);
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            throw new RuntimeException(e);
        }
    }

    private <T,U,V> Consumer<Transition<T,U,V>> loggingConsumer(String message) {
        return t -> logger.info(message+": "+t);
    }

    private Predicate<UnitTest> predicate() {
        return u -> u.bool[0];
    }

    static enum State {
        A, B, C, D
    }
    static enum Event {
        A1, A2, B1, B2, C1
    }
}
