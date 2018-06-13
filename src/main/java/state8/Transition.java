package state8;

import java.util.function.Consumer;
import java.util.function.Predicate;

public class Transition<T, U, V> {

    private final State<T, U, V> current;
    private final V event;
    private final Predicate<U> guard;
    private final State<T, U, V> next;
    private final U ctx;

    protected Transition(State<T,U,V> current, U ctx, V event, Predicate<U> guard, State<T, U, V> next) {
        this.current = current;
        this.ctx = ctx;
        this.event = event;
        this.guard = guard;
        this.next = next;
    }

    public T current() {
        return current.nested;
    }

    public V event() {
        return event;
    }

    public Predicate<U> guard() {
        return guard;
    }

    public T next() {
        return next.nested;
    }

    protected State<T,U,V> apply(U ctx) {
        if(guard().test(ctx)) {
            current.onExit(this);
            return next.onEntry(this);
        }
        return current;
    }

    protected U context() {
        return ctx;
    }

    public static class Builder<T, U, V> {

        private final StateMachine.Builder<T, U, V> parent;
        protected V event;
        private Predicate<V> guard;
        private State<T,U,V> next;
        private Consumer<Transition<T, U, V>> consumer = t -> {};

        public Builder(StateMachine.Builder<T, U, V> parent) {
            this.parent = parent;
            this.parent.currentState().transition(this);
        }

        public Transition.Builder onEvent(V event) {
            this.event = event;
            return this;
        }

        public Builder<T, U, V> unless(Predicate<V> guard) {
            this.guard = guard;
            return this;
        }

        public Builder<T, U, V> goTo(T next) {
            this.next = this.parent.stateFor(next);
            return this;
        }

        protected Transition<T, U, V> build() {
            return new Transition(this.parent.currentState(), this.parent.context(), this.event, this.guard, this.next);
        }

        public Builder<T, U, V> onTransition(Consumer<Transition<T,U,V>> consumer) {
            this.consumer = consumer;
            return this;
        }

        public StateMachine.Builder<T, U, V> then() {
            return this.parent;
        }

        public StateMachine.Builder<T, U, V> and() {
            return this.parent;
        }
    }
}
