package dzone;

public interface Either<A, B> {

    boolean isLeft();

    boolean isRight();

    A getLeft();

    B getRight();

    static <A, B> Either<A, B> right(B value) {
        return new Right<>(value);
    }

    static <A, B> Either<A, B> left(A value) {
        return new Left<>(value);
    }

    public class Left<A, B> implements Either<A, B> {

        private final A left;

        private Left(A left) {
            super();
            this.left = left;
        }

        @Override
        public boolean isLeft() {
            return true;
        }

        @Override
        public boolean isRight() {
            return false;
        }

        @Override
        public A getLeft() {
            return this.left;
        }

        @Override
        public B getRight() {
            throw new IllegalStateException("getRight() called on Left value");
        }

        @Override
        public String toString() {
            return left.toString();
        }
    }

    public class Right<A, B> implements Either<A, B> {

        private final B right;

        private Right(B right) {
            super();
            this.right = right;
        }

        @Override
        public boolean isLeft() {
            return false;
        }

        @Override
        public boolean isRight() {
            return true;
        }

        @Override
        public A getLeft() {
            throw new IllegalStateException("getLeft() called on Right value");
        }

        @Override
        public B getRight() {
            return this.right;
        }

        @Override
        public String toString() {
            return right.toString();
        }
    }
}
