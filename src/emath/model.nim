import std/tables

type
  EMathOperator* = enum
    # -- operation
    emoPow = "^"
    emoMult = "*"
    emoDiv = "/"
    emoPlus = "+"
    emoMinus = "-"
    emoMod = "%"
    # -- logical
    emoNotFact = "!"
    emoAnd = "&"
    emoOr = "|"
    # -- comparison
    emoLarger = ">"
    emoLargerEq = ">="
    emoEq = "=="
    emoNotEq = "!="
    emoAlmostEq = "~="
    emoLessEq = "<="
    emoLess = "<"
    # -- others
    emoAssign = "="

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

  EMathFn* = proc(args: seq[float]): float {.noSideEffect.}
  EMathFnLookup* = Table[string, EMathFn]
  EMathVarLookup* = Table[string, float]


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
  of emoPow: 8
  of emoMult, emoDiv: 7
  of emoPlus, emoMinus: 6
  of emoMod: 5
  of emoNotFact: 4
  of emoAnd: 3
  of emoOr: 2
  of emoLarger, emoLargerEq, emoAlmostEq, emoEq, emoNotEq, emoLessEq, emoLess: 1
  of emoAssign: 0
