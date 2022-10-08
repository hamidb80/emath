import std/tables

type
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
    mtkOperator
    mtkIdent
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

  MathFn* = proc(args: seq[float]): float {.nosideeffect.}
  MathFnLookup* = Table[string, MathFn]
  MathVarLookup* = Table[string, float]



func toMathNode*(f: float): MathNode =
  MathNode(kind: mnkLit, value: f)


func priority*(mo: MathOperator): int =
  case mo:
  of mlkPow: 4
  of mokMult, mokDiv: 3
  of mokPlus, mokminus: 2
  of mokMod: 1
  of mokLarger, mokLargerEq, mokEq, mokLessEq, mokLess: 0
