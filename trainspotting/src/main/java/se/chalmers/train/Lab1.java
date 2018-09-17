package se.chalmers.train;

import java.util.ArrayList;
import java.util.concurrent.LinkedBlockingQueue;

import se.chalmers.train.TSim.*;
import se.chalmers.train.Train;

public class Lab1 {
  private LinkedBlockingQueue<Thread> trains = new LinkedBlockingQueue<Thread>();

  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();

    try {
      // Section 1
      Sensor s1 = new Sensor(5, 13, SwitchDirection.SWTICH_SOUTH_LEFT);
      Sensor s2 = new Sensor(5, 11, SwitchDirection.SWTICH_SOUTH_RIGHT);
      ArrayList<Sensor> southSensors = new ArrayList<>();
      southSensors.add(s1);
      southSensors.add(s2);

      Sensor s3 = new Sensor(5, 10, SwitchDirection.SWTICH_NORTH_RIGHT);
      Sensor s4 = new Sensor(5, 9, SwitchDirection.SWTICH_NORTH_LEFT);
      ArrayList<Sensor> northSensors = new ArrayList<>();
      northSensors.add(s3);
      northSensors.add(s4);

      Switch swS1 = new Switch(3, 11, tsi);
      Switch swN2 = new Switch(4, 9, tsi);

      // Section 2
      // South
      Sensor s5 = new Sensor(12, 10, SwitchDirection.SWTICH_SOUTH_RIGHT);
      Sensor s6 = new Sensor(12, 9, SwitchDirection.SWTICH_SOUTH_LEFT);
      ArrayList<Sensor> southSensorsCS2 = new ArrayList<>();
      southSensorsCS2.add(s5);
      southSensorsCS2.add(s6);
      // North
      Sensor s7 = new Sensor(15, 8, SwitchDirection.SWTICH_SOUTH_LEFT);
      Sensor s8 = new Sensor(15, 7, SwitchDirection.SWTICH_SOUTH_LEFT);
      ArrayList<Sensor> northSensorsCS2 = new ArrayList<>();
      northSensorsCS2.add(s7);
      northSensorsCS2.add(s8);

      Switch swS3 = new Switch(15, 9, tsi);
      Switch swN4 = new Switch(17, 7, tsi);

      // TODO: add both switches to CS + sensors, then keep track of active /
      // inactive & determine direction => which way to guide train..
      // NORTH vs SOUTH sensor hit.
      CriticalSection criticalSection = new CriticalSection(northSensors, southSensors, swN2, swS1);
      CriticalSection criticalSection1 = new CriticalSection(northSensorsCS2, southSensorsCS2, swN4, swS3);

      CriticalSection[] criticalSections = new CriticalSection[] { criticalSection, criticalSection1 };

      Thread train1 = new Thread(new Train(1, tsi, speed1, Train.ANSI_BLUE, criticalSections));
      Thread train2 = new Thread(new Train(2, tsi, speed2, Train.ANSI_PURPLE, criticalSections));

      // Apparently swtiches are facing in different directions...
      // Depending on their location on the board.
      swS1.setDirection(SwitchDirection.SWTICH_SOUTH_LEFT);
      swN2.setDirection(SwitchDirection.SWTICH_SOUTH_LEFT);
      swS3.setDirection(SwitchDirection.SWTICH_NORTH_LEFT);
      swN4.setDirection(SwitchDirection.SWTICH_NORTH_LEFT);

      // try {
      // tsi.setSwitch(3, 11, SwitchNorthDirection.SWITCH_LEFT.ordinal());
      // tsi.setSwitch(4, 9, SwitchNorthDirection.SWITCH_LEFT.ordinal());
      // tsi.setSwitch(15, 9, SwitchNorthDirection.SWITCH_LEFT.ordinal());
      // tsi.setSwitch(17, 7, SwitchNorthDirection.SWITCH_LEFT.ordinal());
      // } catch (Exception err) {
      // err.printStackTrace();
      // }

      train1.start();
      train2.start();
      train1.join();
      train2.join();

    } catch (InterruptedException err) {
      err.printStackTrace();
    }
  }
}
