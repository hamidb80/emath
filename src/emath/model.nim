import std/tables

type
  MathOperator* = enum
    # -- operation
    mokPow = "^"
    mokMult = "*"
    mokDiv = "/"
    mokPlus = "+"
    mokMinus = "-"
    mokMod = "%"
    # -- logical
    mokNotFact = "!"
    mokAnd = "&"
    mokOr = "|"
    # -- comparison
    mokLarger = ">"
    mokLargerEq = ">="
    mokEq = "=="
    mokAlmostEq = "~="
    mokLessEq = "<="
    mokLess = "<"

  MathTokenKind* = enum
    mtkNumber
    mtkOperator, mtkIdent
    mtkOpenPar, mtkClosePar
    mtkComma

  MathToken* = object
    slice*: Slice[int]

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
    mnkPrefix, mnkPostfix, mnkInfix

  MathNode* {.acyclic.} = ref object
    children*: seq[MathNode]
    isFinal*: bool ## used for parsing and validity check

    case kind*: MathNodeKind
    of mnkLit:
      value*: float

    of mnkCall, mnkVar:
      ident*: string

    of mnkPrefix, mnkPostfix, mnkInfix:
      operator*: MathOperator

    of mnkPar: discard

  MathFn* = proc(args: seq[float]): float {.noSideEffect.}
  MathFnLookup* = Table[string, MathFn]
  MathVarLookup* = Table[string, float]


func left*(mn: MathNode): MathNode =
  ## returns the left side of an infix operator
  assert mn.kind == mnkInfix
  mn.children[0]

func right*(mn: MathNode): MathNode =
  ## returns the right side of an infix operator
  assert mn.kind == mnkInfix
  mn.children[1]

func inside*(mn: MathNode): MathNode =
  ## returns the inside value of a parenthesis or a prefix
  assert mn.kind in {mnkPrefix, mnkPostfix, mnkPar}
  mn.children[0]


func priority*(mo: MathOperator): int =
  case mo
  of mokPow: 6
  of mokMult, mokDiv: 5
  of mokPlus, mokMinus: 4
  of mokMod: 3
  of mokNotFact: 2
  of mokAnd, mokOr: 1
  of mokLarger, mokLargerEq, mokAlmostEq, mokEq, mokLessEq, mokLess: 0
