package se.chalmers.train;

import se.chalmers.train.TSim.TSimInterface;

public class Switch {
    SwitchDirection direction;
    TSimInterface tsx;
    int x, y;

    public Switch(int x, int y, TSimInterface tsx) {
        this.x = x;
        this.y = y;
        this.tsx = tsx;
    }

    /**
     * @param direction the direction to set
     */
    public void setDirection(SwitchDirection direction) {
        try {
            this.tsx.setSwitch(this.x, this.y, direction.getValue());
            this.direction = direction;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}