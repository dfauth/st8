package dzone;

public class Outcome {

    public final Integer account;
    public final List<Either<Exception, Integer>> operations;

    public Outcome(Integer account, List<Either<Exception, Integer>> operations) {
        super();
        this.account = account;
        this.operations = operations;
    }

    public String toString() {
        return "(" + account.toString() + "," + operations.toString() + ")";
    }
}
