package se.chalmers.train;

import java.util.concurrent.*;
import java.util.*;
import java.awt.*;

import se.chalmers.train.TSim.*;

public class Train implements Runnable {

    private int id;
    private int speed;
    private ArrayList<Section> map;
    private TSimInterface tsi;

    public Train(int id, int speed, ArrayList<Section> map, TSimInterface tsi) {
        this.id = id;
        this.speed = speed;
        this.map = map;
        this.tsi = tsi;
    }

    public void run() {
        while (true) {
            try {
                SensorEvent event = tsi.getSensor(id);
                Section location = getSection(map, event);
                if (location != null)
                    resolve(location, event);
            } catch (InterruptedException | CommandException e) {
                e.printStackTrace();
                System.exit(1);
            }
        }
    }

    private Section getSection(ArrayList<Section> map, SensorEvent event)
            throws CommandException, InterruptedException {
        Point sensor = new Point(event.getXpos(), event.getYpos());
        for (Section s : map) {
            if (s.getSensors().contains(sensor))
                return s;
        }
        return null;
    }

    private void resolve(Section section, SensorEvent event) throws InterruptedException, CommandException {
        Semaphore sem = section.getSemaphore();
        Point sen = new Point(event.getXpos(), event.getYpos());
        Point swi = section.getSwitch(sen);

        if (event.getStatus() == SensorEvent.ACTIVE) {
            if (sem.tryAcquire()) {
                section.setTrain(this);
                System.out.println("semaphore acquired");
                // tsi.setSwitch(swi.x, swi.y, TSimInterface.SWITCH_RIGHT);
            } else if (!sem.tryAcquire() && section.getTrain().equals(this)) {
                sem.release();
                System.out.println("semaphore released");
            } else if (!sem.tryAcquire() && section.getType() == Intersection.FORK) {
                // tsi.setSwitch(swi.x, swi.y, TSimInterface.SWITCH_LEFT);
                System.out.println("detour selected");
            } else {
                System.out.println("stopped");
                stop();
                sem.acquire();
                section.setTrain(this);
                setSpeed(speed);
            }
        }
    }

    private void setSpeed(int speed) throws CommandException, InterruptedException {
        this.speed = speed;
        tsi.setSpeed(this.id, this.speed);
    }

    private void stop() throws CommandException, InterruptedException {
        tsi.setSpeed(this.id, 0);
    }

}