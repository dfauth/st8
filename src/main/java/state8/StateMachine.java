package state8;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

public class StateMachine<T, U, V> {

    private final String name;
    private final U ctx;
    private State<T, U, V> current;

    private StateMachine(String name, State<T,U,V> current, U ctx) {
        this.name = name;
        this.current = current;
        this.ctx = ctx;
    }

    public static StateMachine.Builder create(String name) {
        return create(name, null);
    }

    public static <V> StateMachine.Builder create(String name, V ctx) {
        return new StateMachine.Builder(name, ctx);
    }

    public T current() {
        return this.current.nested;
    }

    public StateMachine<T, U, V> trigger(V event) {
        return trigger(event, ctx);
    }

    private StateMachine<T, U, V> trigger(V event, U ctx) {
        current.transitionsFor(event).forEach(t -> {
            this.current = t.apply(ctx);
        });
        return this;
    }

    public static class Builder<T, U, V> {

        private final String name;
        private final U ctx;
        private State<T, U, V> initial;
        private State<T,U,V> current;
//        private Map<Key, Transition<T, U, V>> transitions = new HashMap<>();
        private Map<T, State<T, U, V>> states = new HashMap<>();

        public Builder(String name, U ctx) {
            this.name = name;
            this.ctx = ctx;
        }

        public Builder<T, U, V> initial(T initial) {
            state(initial);
            this.initial = this.current;
            return this;
        }

        protected State<T, U, V> stateFor(T state) {
            State<T, U, V> s = states.get(state);
            if(s == null) {
                s = new State<>(state);
                states.put(state, s);
            }
            return s;
        }

        public StateMachine<T, U, V> build() {
            states.values().forEach(s -> s.build());
            return new StateMachine<T,U,V>(this.name, this.initial, this.ctx);
        }

        public T current() {
            return this.currentState().nested;
        }

        protected State<T, U, V> currentState() {
            return current;
        }

        public Builder<T, U, V> state(T state) {
            this.current = stateFor(state);
            return this;
        }

        public Transition.Builder<T, U, V> from(T t) {
            state(t);
            return new Transition.Builder<T,U,V>(this);
        }

        public Builder<T, U, V> onEntry(Consumer<Transition<T, U, V>> consumer) {
            this.current.addOnEntryConsumer(consumer);
            return this;
        }

        public Builder<T, U, V> onExit(Consumer<Transition<T, U, V>> consumer) {
            this.current.addOnExitConsumer(consumer);
            return this;
        }

        public U context() {
            return ctx;
        }
    }

}
