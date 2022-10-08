import std/[tables, strutils, sequtils]

type
  MathOperator* = enum
    mlkPow = "^"
    mokMult = "*"
    mokDiv = "/"
    mokPlus = "+"
    mokmonus = "-"
    mokMod = "%"

    mokLarger = ">"
    mokLargerEq = ">="
    mokEq = "=="
    mokLessEq = "<="
    mokLess = "<"

  MathTokenKind = enum
    mtkNumber
    mtkOperator
    mtkIdent
    mtkOpenPar, mtkClosePar
    mtkComma

  MathToken = object
    case kind: MathTokenKind
    of mtkNumber:
      number: float
    of mtkIdent:
      ident: string
    of mtkOperator:
      operator*: MathOperator
    else: discard

  MathNodeKind* = enum
    mnkLit
    mnkPar
    mnkIdent, mnkFn
    mnkPrefix, mnkInfix

  MathNode* = ref object
    children*: seq[MathNode]

    case kind*: MathNodeKind
    of mnkLit:
      value*: float

    of mnkFn, mnkIdent:
      ident*: string

    of mnkPrefix, mnkInfix:
      operator*: MathOperator

    of mnkPar: discard

  MathFn* = proc(args: seq[float]): float
  MathFnLookup* = Table[string, MathFn]
  MathVarLookup* = Table[string, float]


func `$`*(mn: MathNode): string =
  case mn.kind:
  of mnkLit: $mn.value
  of mnkPar: '(' & $mn.children[0] & ')'
  of mnkIdent: mn.ident
  of mnkFn: mn.ident & '(' & mn.children.map(`$`).join(", ") & ')'
  of mnkPrefix: $mn.operator & $mn.children[0]
  of mnkInfix: $mn.children[0] & ' ' & $mn.operator & ' ' & $mn.children[1]


func priority(mo: MathOperator): int =
  case mo:
  of mlkPow: 4
  of mokMult, mokDiv: 3
  of mokPlus, mokmonus: 2
  of mokMod: 1
  of mokLarger, mokLargerEq, mokEq, mokLessEq, mokLess: 0

func putPars*(mn: MathNode): MathNode =
  ## 1+2*3 == (1+(2*3))
  discard

func eval*(mn: MathNode,
  varLookup: MathVarLookup,
  fnLookup: MathFnLookup): float =

  discard


type MathLexerState = enum
  mlsReady
  mlsInt, mlsFloat
  mlsOperator
  mlsIdent

const
  Operators = {'+', '-', '*', '/', '^', '=', '<', '>', '%'}
  EoS = '\0' # End of Sting

template mtoken(k: MathTokenKind): untyped =
  MathToken(kind: k)

template lexError(msg): untyped =
  raise newException(ValueError, msg)

iterator lex(input: string): MathToken =
  var
    i = 0
    anchor = 0
    state = mlsReady

  while i < input.len + 1:
    let ch =
      if i == input.len: EoS
      else: input[i]

    template enterState(s): untyped =
      anchor = i
      state = s

    template twist: untyped =
      case state:
      of mlsFloat, mlsInt:
        yield MathToken(kind: mtkNumber, number: parseFloat input[anchor ..< i])
      of mlsIdent:
        yield MathToken(kind: mtkIdent, ident: input[anchor ..< i])
      of mlsOperator:
        yield MathToken(kind: mtkOperator,
          operator: parseEnum[MathOperator](input[anchor ..< i]))

      else: lexError "??"

      state = mlsReady
      continue

    template onReady(body): untyped =
      case state
      of mlsReady: body
      else: twist

    case ch
    of Whitespace, EoS:
      case state:
      of mlsReady: discard
      else: twist

    of Operators:
      case state
      of mlsOperator:
        if ch notin {'<', '>', '='}: twist
        else: discard

      of mlsReady: enterState mlsOperator
      else: twist

    of Digits:
      case state
      of mlsReady: enterState mlsInt
      of mlsInt, mlsFloat, mlsIdent: discard
      else: twist

    of Letters:
      case state
      of mlsIdent: discard
      of mlsReady: enterState mlsIdent
      else: twist

    of '.':
      case state
      of mlsInt: state = mlsFloat
      else: lexError ". ?"

    of ',':
      onReady:
        yield mtoken mtkComma

    of '(':
      onReady:
        yield mtoken mtkOpenPar

    of ')':
      onReady:
        yield mtoken mtkClosePar

    else:
      lexError "invalid character: " & ch


    inc i


func toMathNode(f: float): MathNode =
  MathNode(kind: mnkLit, value: f)


proc parse*(input: string): MathNode =
  var stack: seq[MathNode]

  for t in lex input:
    echo t
    
    case t.kind:
    of mtkNumber:
      let t = toMathNode t.number

      # if result == nil:
      # else:

    of mtkOperator:
      let temp = MathNode(kind: mnkInfix, operator: t.operator)

      if result == nil: # prefix
        discard

      else:
        case result.kind:
        of mnkInfix:
          # if target.operator.priority <= temp.operator.priority:
          #   discard

          # else:
            discard

        else:
          temp.children.add result
          result = temp

    of mtkIdent: discard
    of mtkOpenPar: discard
    of mtkClosePar: discard
    of mtkComma: discard

  stack[0]


when isMainModule:
  echo parse "1 + 2 * 3"
  # discard parse "1 + 2 ^ 0.3 / 4 (9==) sin(log10(3.14))"
