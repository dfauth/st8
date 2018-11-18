package dzone;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Timer {

    private static final Logger logger = LogManager.getLogger(Timer.class);

    public static long timed(Runnable runnable) {
        long then = System.nanoTime();
        try {
            runnable.run();
        } catch (Throwable t){
            logger.error(t.getMessage(),t);
        } finally {
          return ((System.nanoTime() - then)/1000000);
        }
    }


}
