(:debug :background)
function spaceSeparated() as Void {}

(:debug, :background)
function commaSeparated() as Void {}

(:test :debug, :background)
function mixedSeparators() as Void {}

(:a /* first */ :b)
function commentBetween() as Void {}
