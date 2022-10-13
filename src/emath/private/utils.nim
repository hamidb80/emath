import ../model, ../exceptions

# --- errors

template evalErr*(msg): untyped =
  raise newException(ValueError, msg)

template lexError*(msg): untyped =
  raise newException(ValueError, msg)

template parserErr*(msg): untyped =
  raise newException(ValueError, msg)

# --- conventions

template last*(s: seq): untyped =
  s[^1]

template mtoken*(k: MathTokenKind): untyped =
  MathToken(kind: k)

# --- math node

func newPrefix*(o: MathOperator): MathNode =
  MathNode(kind: mnkPrefix, operator: o)

func newInfix*(o: MathOperator): MathNode =
  MathNode(kind: mnkInfix, operator: o)

func newVar*(i: string): MathNode =
  MathNode(kind: mnkVar, ident: i)

func newPar*: MathNode =
  MathNode(kind: mnkPar, isFinal: false)

func newCall*(i: string): MathNode =
  MathNode(kind: mnkCall, ident: i, isFinal: false)

func newLiteral*(f: float): MathNode =
  MathNode(kind: mnkLit, value: f)

# --- others

func isInt*(f: float): bool =
  f.int.float == f
