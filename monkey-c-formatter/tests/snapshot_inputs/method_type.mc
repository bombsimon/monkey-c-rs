import Toybox.Lang;

function doWork(x as Method(a as Number) as String) as String {
    return x.invoke(2);
}

function noReturn(cb as Method(s as String)) {}

function multi(cb as Method(x as Number, y as Number) as Boolean) {}

typedef Cb as Method(x as Number, y as Number) as Boolean;

typedef Optional as Method(s as String)?;

function unionReturn(cb as Method(x as Number) as Number or Null) {}

class C {
    function attach(handler as Method(event as Object) as Void) {}
}
