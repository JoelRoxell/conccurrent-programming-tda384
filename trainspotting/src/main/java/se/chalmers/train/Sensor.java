package se.chalmers.train;

public class Sensor {
    private int x;
    private int y;
    private boolean active;
    private int triggeredByTrain;
    private SwitchDirection direction;

    public Sensor(int x, int y, SwitchDirection entryDirection) {
        this.x = x;
        this.y = y;
        this.setState(false);
        this.setTriggeredByTrain(-1);
        this.setDirection(entryDirection);
    }

    /**
     * @return the direction
     */
    public SwitchDirection getEntryDirection() {
        return direction;
    }

    /**
     * @param direction the direction to set
     */
    public void setDirection(SwitchDirection direction) {
        this.direction = direction;
    }

    /**
     * @return the triggeredByTrain
     */
    public int getTriggeredByTrain() {
        return triggeredByTrain;
    }

    /**
     * @param triggeredByTrain the triggeredByTrain to set
     */
    public void setTriggeredByTrain(int triggeredByTrain) {
        this.triggeredByTrain = triggeredByTrain;
    }

    /**
     * @return the state
     */
    public boolean isActive() {
        return active;
    }

    /**
     * @param state the state to set
     */
    public void setState(boolean active) {
        this.active = active;
    }

    /**
     * @return the y
     */
    public int getY() {
        return y;
    }

    /**
     * @return the x
     */
    public int getX() {
        return x;
    }
}