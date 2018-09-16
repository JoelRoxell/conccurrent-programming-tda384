package se.chalmers.train;

import java.util.concurrent.*;
import java.util.*;
import java.awt.*;

import se.chalmers.train.TSim.*;
import se.chalmers.train.Train;

public class Lab1 {

  public Lab1(int speed1, int speed2) {

    TSimInterface tsi = TSimInterface.getInstance();

    /*
     * Hardwires the layout of the map and its different elements into map
     * structures for component connectivity and easy access.
     * 
     */

    Semaphore[] sem_index = new Semaphore[9];

    for (int i = 0; i < sem_index.length; i++)
      sem_index[i] = new Semaphore(1);

    Map<Point, Semaphore[]> sens = new HashMap<>();

    Semaphore[] sems1 = { sem_index[4], sem_index[3] };
    Semaphore[] sems2 = { sem_index[0], sem_index[1] };
    Semaphore[] sems3 = { sem_index[6], sem_index[7] };
    Semaphore[] sems4 = { sem_index[2] };
    Semaphore[] sems5 = { sem_index[5] };
    Semaphore[] sems6 = { sem_index[8] };

    sens.put(new Point(17, 9), sems1);
    sens.put(new Point(2, 9), sems1);
    sens.put(new Point(1, 11), sems2);
    sens.put(new Point(19, 7), sems3);
    sens.put(new Point(5, 11), sems4);
    sens.put(new Point(4, 13), sems4);
    sens.put(new Point(6, 10), sems4);
    sens.put(new Point(6, 9), sems4);
    sens.put(new Point(13, 10), sems5);
    sens.put(new Point(15, 7), sems5);
    sens.put(new Point(15, 8), sems5);
    sens.put(new Point(13, 9), sems5);
    sens.put(new Point(10, 7), sems6);
    sens.put(new Point(10, 8), sems6);
    sens.put(new Point(6, 7), sems6);
    sens.put(new Point(8, 5), sems6);

    Map<Point, Point> switches = new HashMap<>();

    switches.put(new Point(13, 9), new Point(15, 9));
    switches.put(new Point(13, 10), new Point(15, 9));
    switches.put(new Point(17, 9), new Point(15, 9));
    switches.put(new Point(1, 11), new Point(3, 11));
    switches.put(new Point(5, 11), new Point(3, 11));
    switches.put(new Point(4, 13), new Point(3, 11));
    switches.put(new Point(15, 7), new Point(17, 7));
    switches.put(new Point(15, 8), new Point(17, 7));
    switches.put(new Point(19, 7), new Point(17, 7));
    switches.put(new Point(2, 9), new Point(4, 9));
    switches.put(new Point(6, 9), new Point(4, 9));
    switches.put(new Point(6, 10), new Point(4, 9));

    try {

      Train train1 = new Train(1, speed1, 6, sem_index, sens, switches, tsi);
      Train train2 = new Train(2, speed2, 0, sem_index, sens, switches, tsi);

      Thread thread1 = new Thread(train1);
      Thread thread2 = new Thread(train2);

      tsi.setSpeed(1, speed1);
      tsi.setSpeed(2, speed2);

      thread1.start();
      thread2.start();
      thread1.join();
      thread2.join();

    } catch (InterruptedException | CommandException e) {
      e.printStackTrace();
      System.exit(1);
    }
  }

}