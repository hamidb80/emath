import ../model, ../exceptions

# --- errors

template parseErr*(msg): untyped =
  newException(EMathParseError, msg)

template parseTokErr*(m, s): untyped =
  var e = new EMathTokenError
  e.msg = m & " at indexes: " & $s
  e.slice = s
  e

template evalErr*(msg): untyped =
  raise newException(EMathEvalError, msg)

template undefinedErr*(name: string, k: MathSymbolKind): untyped =
  var e = new EMathNotDefined
  e.msg = "the " & $k & " '" & name & "' is not defined"
  e.ident = name
  e.kind = k
  e

# --- conventions

template last*(s: seq): untyped =
  s[^1]

template first*(s: seq): untyped =
  s[0]

# --- numbers

func isInt*(f: float): bool =
  f.toInt.toFloat == f

func toBinary*(f: float): bool =
  f != 0.0

# --- math node

func newPrefix*(o: MathOperator): MathNode =
  MathNode(kind: mnkPrefix, operator: o)

func newPostfix*(o: MathOperator, sub: MathNode): MathNode =
  MathNode(kind: mnkPostfix, operator: o, children: @[sub])

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


func isOpenWrapper*(mn: MathNode): bool =
  (mn.kind in {mnkPar, mnkCall}) and (not mn.isFinal)

func isFinalValue*(mn: MathNode): bool =
  mn.kind in {mnkLit, mnkVar, mnkPostfix} or
  mn.kind in {mnkPar, mnkCall} and mn.isFinal
