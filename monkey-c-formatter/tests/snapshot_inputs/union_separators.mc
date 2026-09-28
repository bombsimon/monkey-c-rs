typedef Keyword as Number or Float;
typedef Pipe as Number | Float;
typedef Mixed as String or Number | Float;

function f(value as Array<Number | Null>) as String or Null {
    var cast = value as Number | Null;
    var mixedCast = value as Number or Float | Null;
}
