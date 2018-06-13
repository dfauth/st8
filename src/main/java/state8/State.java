package state8;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

public class State<T, U, V> {
    final T nested;
    private Consumer<Transition<T, U, V>> onEntryConsumer = (t) -> {};
    private Consumer<Transition<T, U, V>> onExitConsumer = (t) -> {};
    private Multimap<V, Transition<T, U, V>> transitions = ArrayListMultimap.create();
    private List<Transition.Builder<T,U,V>> builders = new ArrayList<>();

    public State(T state) {
        this.nested = state;
    }

    public void addOnEntryConsumer(Consumer<Transition<T, U, V>> consumer) {
        onEntryConsumer.andThen(consumer);
    }

    public void addOnExitConsumer(Consumer<Transition<T, U, V>> consumer) {
        onExitConsumer.andThen(consumer);
    }

    protected State<T, U, V> onEntry(Transition<T, U, V> t) {
        onEntryConsumer.accept(t);
        return this;
    }

    protected State<T, U, V> onExit(Transition<T, U, V> t) {
        onExitConsumer.accept(t);
        return this;
    }

    @Override
    public int hashCode() {
        return this.nested.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if(obj instanceof State) {
            State s = (State) obj;
            return nested.equals(s.nested);
        }
        return false;
    }

    public Iterable<Transition<T, U, V>> transitionsFor(V event) {
        return transitions.get(event);
    }

    public void transition(Transition.Builder<T, U, V> builder) {
        this.builders.add(builder);
    }

    public void build() {
        builders.stream().map(b -> b.build()).forEach(t -> transitions.put(t.event(), t));
    }
}
