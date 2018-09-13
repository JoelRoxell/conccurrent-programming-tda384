package se.chalmers.train;

import java.util.concurrent.*;
import java.util.*;
import java.awt.*;

import se.chalmers.train.TSim.*;
import se.chalmers.train.Train;

public class Lab1 {

  public Lab1(int speed1, int speed2) {

    TSimInterface tsi = TSimInterface.getInstance();
    ArrayList<Section> map = new ArrayList<>();

    Semaphore sem1 = new Semaphore(1);
    Map<Point, Point> sen1 = new HashMap<>();

    sen1.put(new Point(2, 9), new Point(4, 9));
    sen1.put(new Point(17, 9), new Point(15, 9));

    Section sec1 = new Section(sem1, sen1, Intersection.FORK);

    Semaphore sem2 = new Semaphore(1);
    Map<Point, Point> sen2 = new HashMap<>();

    sen2.put(new Point(15, 7), new Point(17, 7));
    sen2.put(new Point(15, 8), new Point(17, 7));
    sen2.put(new Point(13, 9), new Point(15, 9));
    sen2.put(new Point(13, 10), new Point(15, 9));

    Section sec2 = new Section(sem2, sen2, Intersection.ONEWAY);

    map.add(sec1);
    map.add(sec2);

    try {

      Thread train1 = new Thread(new Train(1, speed1, map, tsi));
      Thread train2 = new Thread(new Train(2, speed2, map, tsi));

      tsi.setSpeed(1, speed1);
      tsi.setSpeed(2, speed2);

      train1.start();
      train2.start();
      train1.join();
      train2.join();

    } catch (InterruptedException | CommandException e) {
      e.printStackTrace();
      System.exit(1);
    }
  }

}