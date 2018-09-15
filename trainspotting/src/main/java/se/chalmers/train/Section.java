package se.chalmers.train;

public class Section {

    private boolean forked;
    private Train train;

    public Section(boolean forked) {
        this.forked = forked;
    }

    /**
     * Stores the train currently possesing the section.
     * 
     * @param train
     * 
     */
    public void setTrain(Train train) {
        this.train = train;
    }

    /**
     * Checks wheater the given train is in the section.
     * 
     * @param train
     * @return true if yes, false otherwise
     * 
     */
    public boolean hasTrain(Train train) {
        if (this.train.equals(train))
            return true;
        return false;
    }

    /**
     * States whether this section offers a detour.
     * 
     * @return true if yes, false otherwise
     * 
     */
    public boolean forked() {
        return forked;
    }

}