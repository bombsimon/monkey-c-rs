import Toybox.Lang;
import Toybox.Math;
import Toybox.Application.Storage;

//! A converter class that can convert between speed and pace.
//! Every time either is set, the corresponding value will be updated. This will
//! also set the picker defaults (arrays) so the next time a picker is selected
//! the right defautl values are used.
(:glance)
class SpeedConverter {
    var speed as Float;
    var pace as String;
    var speedPickerDefaults as Array<Number>;
    var pacePickerDefaults as Array<Number>;

    //! Constructor
    function initialize() {
        speed = 0.0;
        pace = "";
        speedPickerDefaults = [0, 0] as Array<Number>;
        pacePickerDefaults = [0, 0] as Array<Number>;

        var defaultPace = Storage.getValue("pace");
        if (defaultPace == null) {
            defaultPace = "5:15";
        }

        // This will set the pace, compuate and set the speed, save the default
        // value and set the values for the pickers.
        setPace(defaultPace as String);
    }

    //! Set the speed and recompute pace. Persist values and update config.
    //! @param _speed The speed in km/h as Float.
    function setSpeed(_speed as Float) as Void {
        speed = _speed;
        updatePace();
        updateConfig();
    }

    //! Set speed based on input from picker (Array of whole number and fraction
    //! as two separate elements).
    function setSpeedWithNumberAndFraction(
        numberAndFraction as Array<Number>
    ) as Void {
        var _speed =
            numberAndFraction[0].toFloat() +
            numberAndFraction[1].toFloat() / 10.0;
        setSpeed(_speed);
    }

    //! Set the pace and recompute speed. Persist values and update config.
    //! @param _pace The speed in min/km as String.
    function setPace(_pace as String) as Void {
        pace = _pace;
        updateSpeed();
        updateConfig();
    }

    //! Set pace based on input from picker (Array of minutes and seconds as two
    //! separate elements).
    function setPaceWithNumbers(minutesAndSeconds as Array<Number>) as Void {
        var pace = Lang.format("$1$:$2$", [
            minutesAndSeconds[0].format("%d"),
            minutesAndSeconds[1].format("%02d"),
        ]);

        setPace(pace);
    }

    //! Update pace will set the pace based on the objects current speed.
    private function updatePace() as Void {
        var _pace = 60 / (speed as Float);
        var minutes = Math.floor(_pace).toNumber();
        var seconds = Math.round((_pace - minutes) * 60);

        pace = Lang.format("$1$:$2$", [
            minutes.format("%02d"),
            seconds.format("%02d"),
        ]);
    }

    //! Update speed will set the speed based on the objects current pace.
    private function updateSpeed() as Void {
        var minutesAndSeconds = splitTime(pace) as Array<Number>;
        var paceInHours =
            minutesAndSeconds[0].toFloat() / 60 +
            minutesAndSeconds[1].toFloat() / 3600;

        speed = 1 / paceInHours;
    }

    //! Update configuration will persist the pace to storage and compute the
    //! default values to use for the pickers based on the current speed and
    //! pace.
    private function updateConfig() as Void {
        Storage.setValue("pace", pace as String);

        var speedWholeNumber = speed.toNumber();
        var speedFraction = Math.round(
            (speed - speedWholeNumber) * 10
        ).toNumber();
        speedPickerDefaults =
            [speedWholeNumber, speedFraction] as Array<Number>;

        pacePickerDefaults = splitTime(pace) as Array<Number>;
    }

    //! Split time into two floats
    //! @param time A string with the pace (or time)
    //! @return An array of two floats.
    private function splitTime(time as String) as Array<Number> {
        var index = time.find(":") as Number;
        var minutes = 0;
        var seconds = 0;

        var minuteString = time.substring(0, index);
        var secondString = time.substring(index + 1, time.length());

        if (minuteString instanceof String) {
            minutes = minuteString.toNumber();
        }

        if (secondString instanceof String) {
            seconds = secondString.toNumber();
        }

        return [minutes, seconds] as Array<Number>;
    }
}
