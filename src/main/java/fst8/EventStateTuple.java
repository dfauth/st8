package fst8;

public class EventStateTuple extends Tuple2<Event, State> {

    public EventStateTuple(Event event, State state) {
        super(event, state);
    }

    public Event event() {
        return _1;
    }

    public State state() {
        return _2;
    }
}
