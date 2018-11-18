package dzone;

import java.util.function.Predicate;


public interface Condition<I, S> extends Predicate<StateTuple<I, S>> {}
