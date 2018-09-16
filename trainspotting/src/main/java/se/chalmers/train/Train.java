package se.chalmers.train;

import java.util.concurrent.*;
import java.util.*;
import java.awt.*;

import se.chalmers.train.TSim.*;

public class Train implements Runnable {

    private int id;
    private int speed;

    private ArrayList<Semaphore> acquired = new ArrayList<Semaphore>();
    private ArrayList<Semaphore> semaphores;
    private Map<Point, Semaphore[]> sensors;
    private Map<Point, Point> switches;

    private TSimInterface tsi;

    public Train(int id, int speed, int sem, Semaphore[] sems, Map<Point, Semaphore[]> sens, Map<Point, Point> switches,
            TSimInterface tsi) {

        this.semaphores = new ArrayList<>(Arrays.asList(sems));

        this.id = id;
        this.speed = speed;
        sensors = sens;
        this.switches = switches;
        this.tsi = tsi;

        try {
            sems[sem].acquire();
            acquired.add(sems[sem]);
        } catch (InterruptedException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    /**
     * Activating a thread with this class will invoke the run method. The thread
     * will repeatedly wait for its corresponding train to trigger a sensor. Upon
     * such an event, the {@link #resolve(SensorEvent) resolve} method will be
     * requested to carry out the actual logic.
     * 
     */
    public void run() {
        while (true) {
            try {
                SensorEvent ev = tsi.getSensor(id);
                resolve(ev);
            } catch (InterruptedException | CommandException e) {
                e.printStackTrace();
                System.exit(1);
            }
        }
    }

    /**
     * The position of the event will identify the corresponding semaphore. If the
     * train is already is possesion of it, the semaphore will be released. If not,
     * the train will try to acquire one of the (possibly two) semaphores. Depending
     * on the one acquired, it will then makes sure that the upcoming switch is in
     * the right direction.
     * 
     * @param ev
     * @throws InterruptedException
     * @throws CommandException
     * 
     */
    private void resolve(SensorEvent ev) throws InterruptedException, CommandException {
        Point sensor = new Point(ev.getXpos(), ev.getYpos());
        Semaphore[] sems = sensors.get(sensor);
        if (ev.getStatus() == SensorEvent.ACTIVE) {
            if (sems == null) {
                reverse();
            } else {
                if (!release(sems)) {
                    acquire(sems);
                    redirect(sensor);
                }
            }
        }
    }

    private boolean release(Semaphore[] sems) {
        for (int i = 0; i < sems.length; i++) {
            if (acquired.contains(sems[i])) {
                acquired.remove(sems[i]);
                sems[i].release();
                return true;
            }
        }
        return false;
    }

    private void acquire(Semaphore[] sems) throws InterruptedException, CommandException {
        if (sems[0].tryAcquire()) {
            acquired.add(sems[0]);
        } else if (sems.length > 1) {
            sems[1].acquire();
            acquired.add(sems[1]);
        } else {
            stop();
            sems[0].acquire();
            acquired.add(sems[0]);
            setSpeed(speed);
        }
    }

    /**
     * Adjust the upcoming switch so to make the train follow the desired path.
     * 
     * @param sensor
     * @param detour
     * @throws CommandException
     * 
     */
    private void redirect(Point sensor) throws CommandException {
        Point swi = switches.get(sensor);
        int track1 = semaphores.indexOf(acquired.get(0));
        int track2 = semaphores.indexOf(acquired.get(1));
        if (swi != null) {
            if ((track1 + track2) % 2 == 0)
                tsi.setSwitch(swi.x, swi.y, TSimInterface.SWITCH_LEFT);
            else
                tsi.setSwitch(swi.x, swi.y, TSimInterface.SWITCH_RIGHT);
        }
    }

    private void setSpeed(int speed) throws CommandException, InterruptedException {
        tsi.setSpeed(this.id, speed);
        this.speed = speed;
    }

    private void stop() throws CommandException, InterruptedException {
        tsi.setSpeed(this.id, 0);
    }

    private void reverse() throws CommandException, InterruptedException {
        stop();
        Thread.sleep(1000 + (20 * Math.abs(speed)));
        setSpeed(-speed);
    }

}