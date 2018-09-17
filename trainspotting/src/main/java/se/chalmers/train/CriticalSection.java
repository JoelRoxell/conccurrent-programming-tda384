package se.chalmers.train;

import java.util.ArrayList;
import java.util.concurrent.Semaphore;

class CriticalSection {
    private ArrayList<Sensor> sensors;
    private Semaphore lock;
    private Switch northSwitch;
    private Switch southSwitch;
    private ArrayList<Sensor> northSensors;
    private ArrayList<Sensor> southSensors;
    private Sensor entrySensor;
    private int trainId;

    public CriticalSection(ArrayList<Sensor> northSensors, ArrayList<Sensor> southSensors, Switch northSwitch,
            Switch southSwitch) {
        this.lock = new Semaphore(1);
        this.northSensors = northSensors;
        this.southSensors = southSensors;
        this.northSwitch = northSwitch;
        this.southSwitch = southSwitch;
        this.setTrainId(-1);
    }

    public boolean notify(int x, int y, int status, int trainId) {
        boolean state = status == 1;

        Sensor sensor = this.findSensor(x, y);
        ArrayList<Sensor> activeSensors = this.getActiveSensors();

        // Sensor is not in the critical section.
        if (sensor == null) {
            return false;
        }

        if (activeSensors.isEmpty()) {
            sensor.setState(state);
            this.entrySensor = sensor;

            if (this.isNorthBond(sensor)) {
                System.out.println(trainId + " is moving south");
                this.northSwitch.setDirection(sensor.getEntryDirection());
            } else {
                System.out.println(trainId + " is moving north");
                this.southSwitch.setDirection(sensor.getEntryDirection());
                // TODO: determine exit strategy.
            }

            this.enterSection(trainId);
        } else {
            System.out.printf("%s train is already in the section\n", this.trainId);

            if (sensor == this.entrySensor) {
                System.out.println(trainId + "same sensor boy");
                // Do nothing
            } else {
                // Check that sensor is an exit sensor, that is, not from the same bound n/s.
                boolean isNorthBound = this.isNorthBond(this.entrySensor);
                boolean exitSensorIsNorthBound = this.isNorthBond(sensor);

                if (isNorthBound == exitSensorIsNorthBound) {
                    // Stop train from entering...
                    System.out.println("From same bound");
                } else {
                    System.out.println(this.trainId + " made a an exit");

                    this.leaveSection();
                }
            }

            // entry or exit?

            // 1. if none is active => aquire lock and turn switches
            // 2 if a previous sensor
        }

        return false;
    }

    private void enterSection(int trainId) {
        try {
            this.lock.acquire();
            this.setTrainId(trainId);
        } catch (Exception e) {
            e.printStackTrace();
            this.leaveSection();
        }
    }

    private void leaveSection() {
        this.setTrainId(-1);
        this.lock.release();
    }

    public Sensor findSensor(int x, int y) {
        Sensor triggeredSensor = null;

        for (Sensor s : this.northSensors) {
            if (x == s.getX() && y == s.getY()) {
                triggeredSensor = s;
            }
        }

        if (triggeredSensor == null) {
            for (Sensor s : this.southSensors) {
                if (x == s.getX() && y == s.getY()) {
                    triggeredSensor = s;
                }
            }
        }

        return triggeredSensor;
    }

    public boolean isNorthBond(Sensor sensor) {
        return this.northSensors.indexOf(sensor) != -1;
    }

    public ArrayList<Sensor> getActiveSensors() {
        ArrayList<Sensor> activeSensors = new ArrayList<>();

        for (Sensor s : this.northSensors) {
            if (s.isActive()) {
                activeSensors.add(s);
            }
        }

        for (Sensor s : this.southSensors) {
            if (s.isActive()) {
                activeSensors.add(s);
            }
        }

        return activeSensors;
    }

    /**
     * @return the northSwitch
     */
    public Switch getNorthSwitch() {
        return northSwitch;
    }

    /**
     * @return the southSwitch
     */
    public Switch getSouthSwitch() {
        return southSwitch;
    }

    /**
     * @return the lock
     */
    public Semaphore getLock() {
        return lock;
    }

    /**
     * @return the trainId
     */
    public int getTrainId() {
        return trainId;
    }

    /**
     * @param trainId the trainId to set
     */
    public void setTrainId(int trainId) {
        this.trainId = trainId;
    }
}
