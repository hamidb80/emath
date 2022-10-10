template last*(s: seq): untyped =
  s[^1]

func isInt*(f: float): bool =
  f.int.float == f
