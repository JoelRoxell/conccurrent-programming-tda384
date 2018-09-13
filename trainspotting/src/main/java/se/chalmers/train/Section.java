package se.chalmers.train;

import java.util.concurrent.*;
import java.util.*;
import java.awt.*;

public class Section {

    private Semaphore semaphore;
    private Map<Point, Point> sensors;
    private Intersection type;
    private Train train = null;

    public Section(Semaphore semaphore, Map<Point, Point> sensors, Intersection type) {
        this.semaphore = semaphore;
        this.sensors = sensors;
        this.type = type;
    }

    public void setTrain(Train train) {
        this.train = train;
    }

    public Train getTrain() {
        return train;
    }

    public Intersection getType() {
        return type;
    }

    public Semaphore getSemaphore() {
        return semaphore;
    }

    public Set<Point> getSensors() {
        return sensors.keySet();
    }

    public Point getSwitch(Point sensor) {
        return sensors.get(sensor);
    }

}