package dzone;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.math.BigInteger;
import java.util.Optional;
import java.util.function.Function;

import static dzone.Timer.timed;


public class UnitTest {

    private static final Logger logger = LogManager.getLogger(UnitTest.class);

    @Test
    public void testIt() {
        logger.info(String.format("fib(42) ran in %d ms",timed(() -> Assert.assertEquals(fib(new BigInteger("42")), new BigInteger("267914296")))));
        logger.info(String.format("fibmemo2(42) ran in %d ms",timed(() -> Assert.assertEquals(fibMemo2(new BigInteger("42")), new BigInteger("267914296")))));
    }

    @Test
    public void doit() {
        List<Input> inputs = List.apply(
                new Deposit(100),
                new Withdraw(50),
                new Withdraw(150),
                new Deposit(200),
                new Withdraw(150));

        StateMonad<Outcome, Outcome> state = Account.createMachine().process(inputs);

        Outcome outcome = state.eval(new Outcome(0, List.empty()));
        logger.info("outcome is : "+outcome);
    }

    static BigInteger fib(BigInteger n) {
        return n.equals(BigInteger.ZERO) || n.equals(BigInteger.ONE)
                ? n
                : fib(n.subtract(BigInteger.ONE)).add(fib(n.subtract(BigInteger.ONE.add(BigInteger.ONE))));
    }

    static BigInteger fibMemo2(BigInteger n) {

        return fibMemo(n).eval(new Memo().addEntry(BigInteger.ZERO, BigInteger.ZERO).addEntry(BigInteger.ONE, BigInteger.ONE));
    }

    static StateMonad<Memo, BigInteger> fibMemo(BigInteger n) {
        return StateMonad.getState((Memo m) -> m.retrieve(n))
                .flatMap(u -> u.map(StateMonad::<Memo, BigInteger> unit).orElse(fibMemo(n.subtract(BigInteger.ONE))
                        .flatMap(x -> fibMemo(n.subtract(BigInteger.ONE).subtract(BigInteger.ONE))
                                .map(x::add)
                                .flatMap(z -> StateMonad.transition((Memo m) -> m.addEntry(n, z), z)))));
    }

    static StateMonad<Memo, BigInteger> fibMemoL(BigInteger n) {

        /*
         * Create a function of type Memo -> Optional<BigInteger> with a closure
         * over the n parameter.
         */
        Function<Memo, Optional<BigInteger>> retrieveValueFromMapIfPresent = (Memo memoizationMap) -> memoizationMap.retrieve(n);

        /*
         * Create a state from this function.
         */
        StateMonad<Memo, Optional<BigInteger>> initialState = StateMonad.getState(retrieveValueFromMapIfPresent);

        /*
         * Create a function for converting the value (BigInteger) into a State
         * Monad instance. This function will be bound to the Optional resulting
         * from the lookup into the map to give the result if the value was found.
         */
        Function<BigInteger, StateMonad<Memo, BigInteger>> createStateFromValue = StateMonad::<Memo, BigInteger> unit;

        /*
         * The value computation proper. This can't be easily decomposed because it
         * make heavy use of closures. It first calls recursively fibMemo(n - 1),
         * producing a StateMonad<Memo, BigInteger>. It then flatMaps it to a new
         * recursive call to fibMemo(n - 2) (actually fibMemo(n - 1 - 1)) and get a
         * new StateMonad<Memo, BigInteger> which is mapped to BigInteger addition
         * with the preceding value (x). Then it flatMaps it again with the function
         * y -> StateMonad.transition((Memo m) -> m.addEntry(n, z), z) which adds
         * the two values and returns a new StateMonad with the computed value added
         * to the map.
         */
        StateMonad<Memo, BigInteger> computedValue = fibMemo(n.subtract(BigInteger.ONE))
                .flatMap(x -> fibMemo(n.subtract(BigInteger.ONE).subtract(BigInteger.ONE))
                        .map(x::add)
                        .flatMap(z -> StateMonad.transition((Memo m) -> m.addEntry(n, z), z)));

        /*
         * Create a function taking an Optional<BigInteger> as its parameter and
         * returning a state. This is the main function that returns the value in
         * the Optional if it is present and compute it and put it into the map
         * before returning it otherwise.
         */
        Function<Optional<BigInteger>, StateMonad<Memo, BigInteger>> computeFiboValueIfAbsentFromMap = u -> u.map(createStateFromValue).orElse(computedValue);

        /*
         * Bind the computeFiboValueIfAbsentFromMap function to the initial State
         * and return the result.
         */
        return initialState.flatMap(computeFiboValueIfAbsentFromMap);
    }
}
