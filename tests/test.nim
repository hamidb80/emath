import std/[unittest, math, tables]
import emath, emath/exceptions, emath/defaults


func `~=`(f1, f2: float): bool =
  0.0001 > abs f1 - f2

template matche(expr, answer): untyped =
  let ast = parse expr
  check answer ~= eval ast


suite "operator priority":
  for (expr, answer) in {
    "1 + 2": 3.0,
    "-1 * -2": 2.0,
    "1 + 2 * 3": 7.0,
    "1 * 2 + 3": 5.0,
    "--1 - 2 - 3": -4.0,
    "2 ^ 8 + -3 * 7": 235.0,
    "-10^2": -100.0,
    "-3! ^ 2 + 1": -35.0,
    "!4!": 0.0,
    "2 != 2": 0.0,
    "2 == 2": 1.0,
  }:
    test expr:
      matche expr, answer

suite "pars":
  const expr = "(((((((1)))))-1)) + (-(-2) * 4) ^ -3 + 2"

  test expr:
    matche expr, 2.001953125

suite "fn call":
  var
    fns = defaultfns
    vars = defaultvars

  fns["nine"] = proc(a: seq[float]): float =
    assert a.len == 0
    9.0

  const expr = "cos(-(0 * 4)) + log(16, 2) - nine() + 10"
  test expr:
    check expr.parse.eval(vars, fns) == +6.0

suite "var":
  test "exists":
    matche "PI*2", TAU

  template checkUndef(k, n, body): untyped =
    var raised = false

    try: body
    except EMathNotDefined:
      raised = true
      var e = (ref EMathNotDefined)(getCurrentException())
      check e.kind == k
      check e.ident == n

    check raised

  test "not exists":
    checkUndef mskVar, "me":
      discard "me + 2 * 3".parse.eval


suite "correctness":
  for expr in [
    ("2^1.1^1.2^1.3 == ((2^1.1)^1.2)^1.3"),
    ("-10^2 == -(10)^2"),
  ]:
    test expr:
      check expr.parse.eval == 1.0


suite "syntax errors":
  test "incomplete":
    for expr in ["(", "1 + "]:
      doAssertRaises EMathParseError:
        discard parse expr


  template checkTokenErr(slc, body): untyped =
    var raised = false

    try: body
    except EMathTokenError:
      raised = true
      let e = (ref EMathTokenError)(getCurrentException())
      check e.slice == slc

    check raised

  test "token":
    for (expr, slc) in {
      "==1": 0..1,
      "(,)": 1..1,
      "1(": 1..1,
      "(2))": 3..3,
      "(2+,)": 3..3,
      "3! 4": 3..3,
    }:
      checkTokenErr slc:
        discard parse expr
