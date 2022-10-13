import std/tables

type
  # TODO add factorial (!) as postfix
  MathOperator* = enum
    # -- operation
    mokPow = "^"
    mokMult = "*"
    mokDiv = "/"
    mokPlus = "+"
    mokminus = "-"
    mokMod = "%"
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

  MathNode* {.acyclic.} = ref object
    children*: seq[MathNode]
    isFinal*: bool ## used for parsing and validity check

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
  assert mn.kind in {mnkPrefix, mnkPar}
  mn.children[0]


func priority*(mo: MathOperator): int =
  case mo:
  of mokPow: 4
  of mokMult, mokDiv: 3
  of mokPlus, mokminus: 2
  of mokMod: 1
  of mokLarger, mokLargerEq, mokAlmostEq, mokEq, mokLessEq, mokLess: 0
