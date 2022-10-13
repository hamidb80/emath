import ../model, ../exceptions

# --- errors

template evalErr*(msg): untyped =
  raise newException(EMathEvalError, msg)

template lexErr*(msg): untyped =
  raise newException(EMathLexError, msg)

template parseErr*(msg): untyped =
  raise newException(EMathParseError, msg)


type MathIdentKind* = enum
  mikFunc = "function"
  mikVar = "variable"

template undefinedErr*(name: string, kind: MathIdentKind): untyped =
  raise newException(EMathNotDefined, "the " & $kind & " '" & name  & "' is not defined")
  

# --- conventions

template last*(s: seq): untyped =
  s[^1]

template first*(s: seq): untyped =
  s[0]


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
