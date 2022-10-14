import std/tables

type
  EMathOperator* = enum
    # -- operation
    emokPow = "^"
    emokMult = "*"
    emokDiv = "/"
    emokPlus = "+"
    emokMinus = "-"
    emokMod = "%"
    # -- logical
    emokNotFact = "!"
    emokAnd = "&"
    emokOr = "|"
    # -- comparison
    emokLarger = ">"
    emokLargerEq = ">="
    emokEq = "=="
    emokNotEq = "!="
    emokAlmostEq = "~="
    emokLessEq = "<="
    emokLess = "<"
    # -- others
    emokAssign = "="

  EMathTokenKind* = enum
    emtkNumber
    emtkOperator, emtkIdent
    emtkOpenPar, emtkClosePar
    emtkComma

  EMathToken* = object
    slice*: Slice[int]

    case kind*: EMathTokenKind
    of emtkNumber:
      number*: float

    of emtkIdent:
      ident*: string

    of emtkOperator:
      operator*: EMathOperator

    else: discard

  EMathNodeKind* = enum
    emnkLit
    emnkPar
    emnkVar, emnkCall
    emnkPrefix, emnkPostfix, emnkInfix

  MathNode* {.acyclic.} = ref object
    children*: seq[MathNode]
    isFinal*: bool ## used for parsing and validity check

    case kind*: EMathNodeKind
    of emnkLit:
      value*: float

    of emnkCall, emnkVar:
      ident*: string

    of emnkPrefix, emnkPostfix, emnkInfix:
      operator*: EMathOperator

    of emnkPar: discard

  MathFn* = proc(args: seq[float]): float {.noSideEffect.}
  MathFnLookup* = Table[string, MathFn]
  MathVarLookup* = Table[string, float]


func left*(mn: MathNode): MathNode =
  ## returns the left side of an infix operator
  assert mn.kind == emnkInfix
  mn.children[0]

func right*(mn: MathNode): MathNode =
  ## returns the right side of an infix operator
  assert mn.kind == emnkInfix
  mn.children[1]

func inside*(mn: MathNode): MathNode =
  ## returns the inside value of a parenthesis/prefix/postfix
  assert mn.kind in {emnkPrefix, emnkPostfix, emnkPar}
  mn.children[0]


func priority*(mo: EMathOperator): int =
  case mo
  of emokPow: 6
  of emokMult, emokDiv: 5
  of emokPlus, emokMinus: 4
  of emokMod: 3
  of emokNotFact: 2
  of emokAnd, emokOr: 1
  of emokLarger, emokLargerEq, emokAlmostEq, emokEq, emokNotEq, emokLessEq, emokLess: 0
  of emokAssign: -1
