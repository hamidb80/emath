import std/tables

type
  # TODO add fact (!) as postfix
  MathOperator* = enum
    # -- operation
    mlkPow = "^"
    mokMult = "*"
    mokDiv = "/"
    mokPlus = "+"
    mokminus = "-"
    mokMod = "%"
    # -- comparison
    mokLarger = ">"
    mokLargerEq = ">="
    mokEq = "=="
    mokLessEq = "<="
    mokLess = "<"

  MathTokenKind* = enum
    mtkNumber
    mtkOperator, mtkIdent
    mtkOpenPar, mtkClosePar
    mtkComma

  MathToken* = object
    case kind*: MathTokenKind
    of mtkNumber:
      number*: float

    of mtkIdent:
      ident*: string

    of mtkOperator:
      operator*: MathOperator

    else: discard

  MathNodeKind* = enum
    mnkLit
    mnkPar
    mnkVar, mnkCall
    mnkPrefix, mnkInfix

  MathNode* = ref object
    children*: seq[MathNode]

    case kind*: MathNodeKind
    of mnkLit:
      value*: float

    of mnkCall, mnkVar:
      ident*: string

    of mnkPrefix, mnkInfix:
      operator*: MathOperator

    of mnkPar: discard

  MathFn* = proc(args: seq[float]): float {.noSideEffect.}
  MathFnLookup* = Table[string, MathFn]
  MathVarLookup* = Table[string, float]


func newPar*: MathNode =
  MathNode(kind: mnkPar)

func newPrefix*(o: MathOperator): MathNode =
  MathNode(kind: mnkPrefix, operator: o)

func newInfix*(o: MathOperator): MathNode =
  MathNode(kind: mnkInfix, operator: o)

func newVar*(i: string): MathNode =
  MathNode(kind: mnkVar, ident: i)

func newCall*(i: string): MathNode =
  MathNode(kind: mnkCall, ident: i)

func newLiteral*(f: float): MathNode =
  MathNode(kind: mnkLit, value: f)


func left*(mn: MathNode): MathNode =
  assert mn.kind == mnkInfix
  mn.children[0]

func right*(mn: MathNode): MathNode =
  assert mn.kind == mnkInfix
  mn.children[1]

func inside*(mn: MathNode): MathNode =
  assert mn.kind in {mnkPrefix, mnkPar}
  mn.children[0]


func priority*(mo: MathOperator): int =
  case mo:
  of mlkPow: 4
  of mokMult, mokDiv: 3
  of mokPlus, mokminus: 2
  of mokMod: 1
  of mokLarger, mokLargerEq, mokEq, mokLessEq, mokLess: 0