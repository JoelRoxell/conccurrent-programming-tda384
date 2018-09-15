package se.chalmers.train;

import java.util.concurrent.*;
import java.util.*;
import java.awt.*;

import se.chalmers.train.TSim.*;

public class Train implements Runnable {

    private int id;
    private int speed;
    private boolean detour = false;

    Map<Semaphore, Section> sections;
    Map<Semaphore, Point[]> semaphores;
    Map<Point, Point[]> switches;

    private TSimInterface tsi;

    public Train(int id, int speed, Map<Semaphore, Section> sections, Map<Semaphore, Point[]> semaphores,
            Map<Point, Point[]> switches, TSimInterface tsi) {
        this.id = id;
        this.speed = speed;
        this.sections = sections;
        this.semaphores = semaphores;
        this.switches = switches;
        this.tsi = tsi;
    }

    /**
     * Activating a thread with this runnable class will invoke this run method. The
     * thread will repeatedly wait for its corresponding train to trigger a sensor.
     * Upon such an event, the method will forwar the information to the
     * {@link #resolve(SensorEvent) resolve} method for further computation.
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
     * This is where the logic takes place. The position of the event will identify
     * the semaphore in question and its corresonding rail section. The following
     * train outcomes are possible:
     * <p>
     * (1) if it acquires the semaphore, the critical section is obtained; (2) if it
     * is already in possesion of the section, the semaphore is released; (3) given
     * the existance of an optional route, it takes a detour; (4) in order to
     * prevent collision, it is forced to stop before acquiring the semaphore.
     * <p>
     * 
     * @param event
     * @throws InterruptedException
     * @throws CommandException
     * 
     */
    private void resolve(SensorEvent event) throws InterruptedException, CommandException {

        Point sen = new Point(event.getXpos(), event.getYpos());
        Semaphore sem = (Semaphore) getKey(semaphores, sen);
        Section sec = sections.get(sem);

        if (event.getStatus() == SensorEvent.ACTIVE) {
            if (sem.tryAcquire()) {
                sec.setTrain(this);
                redirect(sen, detour);
                detour = false;
            } else if (sec.hasTrain(this)) {
                sem.release();
                redirect(sen, false);
            } else if (sec.forked()) {
                detour = true;
                redirect(sen, detour);
            } else {
                stop();
                sem.acquire();
                sec.setTrain(this);
                setSpeed(speed);
            }
        }
    }

    /**
     * Returns the specific key (semaphore or switch) of the map, which values
     * contain the given sensor.
     * 
     * @param sensor
     * @return the corresponding key
     * 
     */
    private <K, V> K getKey(Map<K, V> map, Point sensor) {
        for (K key : map.keySet()) {
            Point[] list = (Point[]) map.get(key);
            if (Arrays.asList(list).contains(sensor)) {
                return key;
            }
        }
        return null;
    }

    /**
     * Adjust the upcoming switch so to make the train follow the desired path
     * decided in the {@link #resolve(SensorEvent) resolve} method.
     * 
     * @param sensor
     * @param detour
     * @throws CommandException
     * 
     */
    private void redirect(Point sensor, boolean detour) throws CommandException {
        Point swi = (Point) getKey(switches, sensor);
        if (swi != null) {
            if (detour && swi.x < 10 || !detour && swi.x > 10)
                tsi.setSwitch(swi.x, swi.y, TSimInterface.SWITCH_RIGHT);
            else
                tsi.setSwitch(swi.x, swi.y, TSimInterface.SWITCH_LEFT);
        }
    }

    /**
     * Alters the speed of the train.
     * 
     * @param speed
     * @throws CommandException
     * @throws InterruptedException
     * 
     */
    private void setSpeed(int speed) throws CommandException, InterruptedException {
        this.speed = speed;
        tsi.setSpeed(this.id, this.speed);
    }

    /**
     * Stops the train.
     * 
     * @throws CommandException
     * @throws InterruptedException
     * 
     */
    private void stop() throws CommandException, InterruptedException {
        tsi.setSpeed(this.id, 0);
    }

    /**
     * Changes the direction of the train.
     * 
     * @throws CommandException
     * @throws InterruptedException
     * 
     */
    private void reverse() throws CommandException, InterruptedException {
        stop();
        Thread.sleep(1000 * speed);
        setSpeed(-speed);
    }

}