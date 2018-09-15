package se.chalmers.train;

import java.util.concurrent.*;
import java.util.*;
import java.awt.*;

import se.chalmers.train.TSim.*;
import se.chalmers.train.Train;

public class Lab1 {

  public Lab1(int speed1, int speed2) {

    TSimInterface tsi = TSimInterface.getInstance();

    Map<Semaphore, Section> sections = new HashMap<>();
    Map<Semaphore, Point[]> semaphores = new HashMap<>();
    Map<Point, Point[]> switches = new HashMap<>();

    /*
     * Hardwires the layout of the map and its different elements into map
     * structures for component connectivity and easy access.
     * 
     */

    Section sec1 = new Section(true);
    Semaphore sem1 = new Semaphore(1);
    Point[] sen1 = { new Point(1, 11) };

    sections.put(sem1, sec1);
    semaphores.put(sem1, sen1);

    Section sec2 = new Section(false);
    Semaphore sem2 = new Semaphore(1);
    Point[] sen2 = { new Point(3, 13), new Point(5, 11), new Point(6, 10), new Point(6, 9) };

    sections.put(sem2, sec2);
    semaphores.put(sem2, sen2);

    Section sec3 = new Section(true);
    Semaphore sem3 = new Semaphore(1);
    Point[] sen3 = { new Point(2, 9), new Point(17, 9) };

    sections.put(sem3, sec3);
    semaphores.put(sem3, sen3);

    Section sec4 = new Section(false);
    Semaphore sem4 = new Semaphore(1);
    Point[] sen4 = { new Point(13, 9), new Point(13, 10), new Point(15, 7), new Point(15, 8) };

    sections.put(sem4, sec4);
    semaphores.put(sem4, sen4);

    Section sec5 = new Section(true);
    Semaphore sem5 = new Semaphore(1);
    Point[] sen5 = { new Point(19, 7) };

    sections.put(sem5, sec5);
    semaphores.put(sem5, sen5);

    Section sec6 = new Section(false);
    Semaphore sem6 = new Semaphore(1);
    Point[] sen6 = { new Point(10, 8), new Point(10, 7), new Point(6, 7), new Point(8, 5) };

    sections.put(sem6, sec6);
    semaphores.put(sem6, sen6);

    Point[] sen7 = { new Point(1, 11), new Point(5, 11), new Point(3, 13) };
    Point[] sen8 = { new Point(2, 9), new Point(6, 9), new Point(6, 10) };
    Point[] sen9 = { new Point(13, 9), new Point(13, 10), new Point(17, 9) };
    Point[] sen10 = { new Point(15, 7), new Point(15, 8), new Point(19, 7) };

    switches.put(new Point(3, 11), sen7);
    switches.put(new Point(4, 9), sen8);
    switches.put(new Point(15, 9), sen9);
    switches.put(new Point(17, 7), sen10);

    try {

      Train train1 = new Train(1, speed1, sections, semaphores, switches, tsi);
      Train train2 = new Train(2, speed2, sections, semaphores, switches, tsi);

      Thread thread1 = new Thread(train1);
      Thread thread2 = new Thread(train2);

      sem1.acquire();
      sec1.setTrain(train2);
      sem5.acquire();
      sec5.setTrain(train1);

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