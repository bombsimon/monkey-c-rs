typedef Model as PercentModel or LabelModel;

typedef ModelUpdater as interface {
    function updateModel() as Complicated.Model;
};

typedef LittleBoys as interface {
    var frogs as Array<Frogs>;
    var snails as Array<Snails>;
    var puppyDogTails as Array<PuppyDogTails>;
};

typedef Mixed as interface {
    var name as String;
    function greet(other as String) as String;
};

typedef ModelUpdater as interface {
    //! Function that provides an updated status
    //! for the complication
    function updateModel() as Complicated.Model;
};

typedef Annotated as interface {
    //! var docs
    var name as String;
    //! function docs
    function greet() as String;
};

function example(you as interface {
    var frogs as Array<Frogs>;
}) {}
