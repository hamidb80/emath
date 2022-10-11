import std/unittest
import emath


func `~=`(f1, f2: float): bool =
  0.01 > abs f1 - f2

template matche(expr, answer): untyped =
  let ast = parse expr
  check:
    isvalid ast
    answer ~= eval ast


suite "operator priority":
  for (expr, answer) in {
    "1 + 2": 3.0,
    "-1 * -2": 2.0,
    "1 + 2 * 3": 7.0,
    "1 * 2 + 3": 5.0,
    "--1 - 2 - 3": -4.0,
    "2 ^ 8 + -3 * 7": 235.0,
    "-10^2": -100.0,
  }:
    test expr:
      matche expr, answer

suite "pars":
  const expr = "(1) + (2 * 3) - 4"

  test expr:
    matche expr, 3.0

suite "fn call":
  const expr = "cos(3.14) + log(16, 2) * 3"
  test expr:
    matche expr, 11

suite "correctness":
  for expr in [
    ("2^1.1^1.2^1.3 == ((2^1.1)^1.2)^1.3"),
    ("-10^2 == -(10)^2"),
  ]:
    test expr:
      check expr.parse.eval == 1.0
