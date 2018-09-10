package se.chalmers.train;

import java.util.concurrent.LinkedBlockingQueue;

import se.chalmers.train.TSim.*;
import se.chalmers.train.Train;

public class Lab1 {
  private LinkedBlockingQueue<Thread> trains = new LinkedBlockingQueue<Thread>();

  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();

    try {
      Thread train1 = new Thread(new Train(1, tsi, speed1, Train.ANSI_BLUE));
      Thread train2 = new Thread(new Train(2, tsi, speed2, Train.ANSI_PURPLE));

      train1.start();
      train2.start();
      train1.join();
      train2.join();

    } catch (InterruptedException err) {
      err.printStackTrace();
    }
  }
}
