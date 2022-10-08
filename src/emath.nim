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

  MathFn* = proc(args: seq[float]): float
  MathFnLookup* = Table[string, MathFn]
  MathVarLookup* = Table[string, float]


func `$`*(mn: MathNode): string =
  case mn.kind:
  of mnkLit: $mn.value
  of mnkPar: '(' & $mn.children[0] & ')'
  of mnkVar: mn.ident
  of mnkCall: mn.ident & '(' & mn.children.map(`$`).join(", ") & ')'
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

# helpers

func isEmpty(s: seq): bool {.inline.} =
  s.len == 0

template last(s: seq): untyped =
  s[^1]

template parserErr(msg): untyped =
  raise newException(ValueError, msg)


proc parse*(input: string): MathNode =
  var stack: seq[MathNode]

  for tk in lex input:
    echo ">> ", tk

    case tk.kind
    of mtkNumber:
      let t = toMathNode tk.number

      if not isEmpty stack:
        case stack.last.kind:
        of mnkInfix, mnkPrefix:
          stack.last.children.add t

        else:
          echo stack
          parserErr "the last is: " & $stack.last.kind

      stack.add t

    of mtkOperator:
      # find general case | not all: --2

      while true:
        if isEmpty stack:
          stack.add MathNode(kind: mnkPrefix, operator: tk.operator)
          break

        elif stack.len == 1:
          stack.add MathNode(kind: mnkInfix, operator: tk.operator, children: @[stack.pop])
          break

        elif stack[^2].kind in {mnkInfix}: # infix
          var temp = MathNode(kind: mnkInfix,
              operator: tk.operator) # FIXME this is not good remember the code graph from "Grokking simplicity"

          case stack[^2].kind:
          of mnkInfix:
            if tk.operator.priority > stack[^2].operator.priority:
              temp.children.add stack[^2].children[1]
              stack[^2].children[1] = temp
              discard stack.pop
              stack.add temp
              break

            else:
              discard stack.pop


          else: discard

        else: # prefix
          discard

    of mtkIdent: discard
    of mtkOpenPar: discard
    of mtkClosePar: discard
    of mtkComma: discard

  stack[0]


func treeReprImpl(mn: MathNode, result: var seq[string], level: int,
    tab = 2)=

  template incl(smth, lvl): untyped =
    result.add indent(smth, lvl * tab)

  template incl(smth): untyped =
    incl smth, level

  template inclChildren(children): untyped =
    for ch in children:
      treeReprImpl ch, result, level + 1

  case mn.kind
  of mnkLit: 
    incl $mn.value

  of mnkPrefix: 
    incl $mn.operator
    inclChildren mn.children

  of mnkInfix: 
    incl $mn.operator
    inclChildren mn.children

  # of mnkPar: "PAR" ... $mn.children[0]
  # of mnkVar: "VAR " & mn.ident
  # of mnkCall: mn.ident & '(' & mn.children.map(`$`).join(", ") & ')'
  else: discard

func treeRepr(mn: MathNode): string =
  var acc: seq[string]
  treeReprImpl mn, acc, 0

  acc.join "\n"

when isMainModule:
  let r = parse "1 + 2 * 3 ^ 4 - 5 * 6 * 7"
  echo r
  echo treerepr r
