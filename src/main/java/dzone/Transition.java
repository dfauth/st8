package dzone;

import java.util.function.Function;


public interface Transition<I, S> extends Function<StateTuple<I, S>, S> {}

