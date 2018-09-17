package se.chalmers.train;

public enum SwitchDirection {
    SWTICH_SOUTH_LEFT(2), SWTICH_SOUTH_RIGHT(1), SWTICH_NORTH_LEFT(1), SWTICH_NORTH_RIGHT(2);

    private final int value;

    SwitchDirection(final int newValue) {
        value = newValue;
    }

    public int getValue() {
        return value;
    }
}
