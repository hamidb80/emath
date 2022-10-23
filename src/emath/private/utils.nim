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

template undefinedErr*(name: string, k: EMathSymbolKind): untyped =
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

func newPrefix*(o: EMathOperator): EMathNode =
  EMathNode(kind: emnkPrefix, operator: o)

func newPostfix*(o: EMathOperator, sub: EMathNode): EMathNode =
  EMathNode(kind: emnkPostfix, operator: o, children: @[sub])

func newInfix*(o: EMathOperator): EMathNode =
  EMathNode(kind: emnkInfix, operator: o)

func newVar*(i: string): EMathNode =
  EMathNode(kind: emnkVar, ident: i)

func newPar*: EMathNode =
  EMathNode(kind: emnkPar, isFinal: false)

func newCall*(i: string): EMathNode =
  EMathNode(kind: emnkCall, ident: i, isFinal: false)

func newLiteral*(f: float): EMathNode =
  EMathNode(kind: emnkLit, value: f)


func isOpenWrapper*(mn: EMathNode): bool =
  (mn.kind in {emnkPar, emnkCall}) and (not mn.isFinal)

func isFinalValue*(mn: EMathNode): bool =
  mn.kind in {emnkLit, emnkVar, emnkPostfix} or
  mn.kind in {emnkPar, emnkCall} and mn.isFinal
