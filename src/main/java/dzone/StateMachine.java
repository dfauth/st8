package dzone;

import java.util.Optional;
import java.util.function.Function;


public class StateMachine<I, S> {

    Function<I, StateMonad<S, Nothing>> function;

    public StateMachine(List<Tuple<Condition<I, S>, Transition<I, S>>> transitions) {
        function = i -> StateMonad.transition(m ->
                Optional.of(new StateTuple<>(i, m)).flatMap((StateTuple<I, S> t) ->
                        transitions.filter((Tuple<Condition<I, S>, Transition<I, S>> x) ->
                                x._1.test(t)).findFirst().map((Tuple<Condition<I, S>, Transition<I, S>> y) ->
                                y._2.apply(t))).get());
    }

    public StateMonad<S, S> process(List<I> inputs) {
        List<StateMonad<S, Nothing>> a = inputs.map(function);
        StateMonad<S, List<Nothing>> b = StateMonad.compose(a);
        return b.flatMap(x -> StateMonad.get());
    }
}