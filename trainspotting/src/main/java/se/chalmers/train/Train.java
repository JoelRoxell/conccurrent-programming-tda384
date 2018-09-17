package se.chalmers.train;

import se.chalmers.train.TSim.*;

public class Train implements Runnable {
    private int trainId;
    private TSimInterface tsi;
    private int speed;
    private String color;
    private CriticalSection[] criticalSecitons;

    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_BLUE = "\u001B[34m";
    public static final String ANSI_PURPLE = "\u001B[35m";
    public static final String ANSI_CYAN = "\u001B[36m";

    public Train(int trainId, TSimInterface tsi, int startingSpeed, String color, CriticalSection[] criticalSecitons) {
        this.trainId = trainId;
        this.tsi = tsi;
        this.setColor(color);
        this.setSpeed(startingSpeed);
        this.criticalSecitons = criticalSecitons;
    }

    /**
     * @return the criticalSecitons
     */
    public CriticalSection[] getCriticalSecitons() {
        return criticalSecitons;
    }

    /**
     * @return the color
     */
    public String getColor() {
        return color;
    }

    /**
     * @param color the color to set
     */
    public void setColor(String color) {
        this.color = color;
    }

    /**
     * @return the speed
     */
    public int getSpeed() {
        return speed;
    }

    /**
     * @param speed the speed to set
     */
    public void setSpeed(int speed) {
        try {
            if (this.tsi == null) {
                throw new Exception("tsi (TSimInterface) must be configured for the Train before a speed can be set.");
            }

            this.speed = speed;
            this.tsi.setSpeed(this.trainId, this.speed);
        } catch (CommandException err) {
            err.printStackTrace();
        } catch (Exception err) {
            err.printStackTrace();
        }
    }

    public void stop() {
        this.setSpeed(0);
    }

    public void start(int speed) throws CommandException {
        this.setSpeed(speed);
    }

    private CriticalSection findSection(int x, int y, int status, int trainsId) {
        for (CriticalSection section : this.criticalSecitons) {
            if (section.notify(x, y, status, trainsId)) {
                return section;
            }
        }

        return null;
    }

    @Override
    public void run() {
        this.print(String.format("Thread controlling: %d started with speed %d", this.trainId, this.speed));

        try {
            while (true) {
                this.print("wating for sensor event...");

                SensorEvent x = (SensorEvent) this.tsi.getSensor(trainId);

                System.out.println(x.getTrainId() + x.getStatus());

                CriticalSection s = this.findSection(x.getXpos(), x.getYpos(), x.getStatus(), x.getTrainId());

                System.out.println(s);
            }
        } catch (CommandException err) {
            err.printStackTrace();
        } catch (InterruptedException err) {
            err.printStackTrace();
        }
    }

    private void print(String message) {
        System.out.format("%s train [%s] %s %s \n", this.color, this.trainId, Train.ANSI_RESET, message);
    }
}