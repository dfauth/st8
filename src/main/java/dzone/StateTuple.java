package dzone;

import java.util.Objects;


public class StateTuple<A, S> {

    public final A value;
    public final S state;

    public StateTuple(A a, S s) {
        value = Objects.requireNonNull(a);
        state = Objects.requireNonNull(s);
    }
}
