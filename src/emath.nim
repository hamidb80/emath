import std/[tables, strutils, sequtils, math, sugar]
import emath/[model, utils, defs]


func `$`*(mn: MathNode): string =
  case mn.kind
  of mnkLit: $mn.value
  of mnkPar: '(' & $mn.children[0] & ')'
  of mnkVar: mn.ident
  of mnkCall: mn.ident & '(' & mn.children.map(`$`).join(", ") & ')'
  of mnkPrefix: $mn.operator & $mn.children[0]
  of mnkInfix: $mn.children[0] & ' ' & $mn.operator & ' ' & $mn.children[1]

func recap(mn: MathNode): string {.used.} =
  case mn.kind
  of mnkLit: "LIT " & $mn.value
  of mnkPar: "PAR"
  of mnkVar: "IDENT " & mn.ident
  of mnkCall: "CALL " & mn.ident
  of mnkPrefix: "PREFIX " & $mn.operator
  of mnkInfix: "INFIX " & $mn.operator

func treeReprImpl(mn: MathNode, result: var seq[string],
  level: int, tab = 2) =

  template incl(smth): untyped =
    result.add indent(smth, level * tab)

  incl:
    case mn.kind
    of mnkLit: "LIT " & $mn.value
    of mnkPrefix: "PREFIX " & $mn.operator
    of mnkInfix: "INFIX " & $mn.operator
    of mnkPar: "PAR"
    of mnkVar: "VAR " & mn.ident
    of mnkCall: "CALL " & mn.ident

  if mn.kind in {mnkPar, mnkCall, mnkPrefix, mnkInfix}:
    for ch in mn.children:
      treeReprImpl ch, result, level + 1

func treeRepr*(mn: MathNode): string =
  ## for debugging purposes
  var acc: seq[string]
  treeReprImpl mn, acc, 0

  acc.join "\n"


func isValid*(mn: MathNode): bool =
  let
    numberOfChildren =
      case mn.kind
      of mnkLit, mnkVar, mnkCall: true
      of mnkPar, mnkPrefix: mn.children.len == 1
      of mnkInfix: mn.children.len == 2

    subNodes =
      case mn.kind
      of mnkLit, mnkVar: true
      of mnkPar, mnkPrefix, mnkCall, mnkInfix:
        mn.children.allIt isValid it

    closed =
      case mn.kind
      of mnkPar, mnkCall: mn.isFinal
      else: true

  numberOfChildren and subNodes and closed


template evalErr(msg): untyped =
  raise newException(ValueError, msg)

func eval*(mn: MathNode,
  varLookup: MathVarLookup,
  fnLookup: MathFnLookup): float =

  template rec(n): untyped =
    eval(n, varLookup, fnLookup)

  case mn.kind
  of mnkLit: mn.value
  of mnkPar: rec mn.children[0]
  of mnkVar: varLookup[mn.ident]
  of mnkCall: fnLookup[mn.ident](mn.children.mapit(rec it))
  of mnkPrefix:
    let v = rec mn.children[0]
    case mn.operator
    of mokPlus: v
    of mokminus: -v
    else: evalErr "invalid prefix"

  of mnkInfix:
    let
      le = rec mn.children[0]
      ri = rec mn.children[1]

    case mn.operator
    of mlkPow: pow(le, ri)
    of mokMult: le * ri
    of mokDiv: le / ri
    of mokPlus: le + ri
    of mokminus: le - ri
    of mokMod:
      assert le.trunc == le
      assert ri.trunc == ri
      float le.int mod ri.int

    of mokLarger: float le > ri
    of mokLargerEq: float le >= ri
    of mokEq: float le == ri
    of mokLessEq: float le <= ri
    of mokLess: float le < ri

func eval*(mn: MathNode): float =
  eval mn, defaultVars, defaultFns


type MathLexerState = enum
  mlsReady
  mlsInt, mlsFloat
  mlsOperator
  mlsIdent

const
  Operators = {'+', '-', '*', '/', '^', '=', '<', '>', '%'}
  EoS = '\0' # End of String

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
      case state
      of mlsFloat, mlsInt:
        yield MathToken(kind: mtkNumber, number: parseFloat input[anchor ..< i])
      of mlsIdent:
        yield MathToken(kind: mtkIdent, ident: input[anchor ..< i])
      of mlsOperator:
        yield MathToken(kind: mtkOperator,
          operator: parseEnum[MathOperator](input[anchor ..< i]))

      else: lexError "invalid state"

      state = mlsReady
      continue

    template onReady(body): untyped =
      case state
      of mlsReady: body
      else: twist

    case ch
    of Whitespace, EoS:
      case state
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


template parserErr(msg): untyped =
  raise newException(ValueError, msg)

func goUp(stack: var seq[MathNode], fn: MathNode -> bool): MathNode =
  ## returns subTree, could be nil
  while true:
    if (fn stack.last):
      return
    else:
      result = stack.pop

  raise newException(ValueError, "couldn't find the desired node")

func parse*(input: string): MathNode =
  var stack: seq[MathNode] = @[newPar()]

  for tk in lex input:
    case tk.kind
    of mtkNumber, mtkIdent:
      let t =
        if tk.kind == mtkNumber:
          newLiteral tk.number
        else:
          newVar tk.ident

      assert stack.last.kind in {mnkInfix, mnkPrefix, mnkPar}
      stack.last.children.add t
      stack.add t

    of mtkOperator:
      if
        (stack.last.kind == mnkPar) and
        (stack.last.children.len == 0) or
        (stack.last.kind in {mnkInfix, mnkprefix}):

        let t = newPrefix tk.operator
        stack.last.children.add t
        stack.add t

      elif stack.last.kind in {mnkLit, mnkVar, mnkPar, mnkCall}:
        var
          t = newInfix tk.operator
          p = t.operator.priority
          n = goUp(stack, (mn: MathNode) =>
            isOpenPar(mn) or
            (mn.kind in {mnkInfix, mnkPrefix}) and
            (p > mn.operator.priority))

        stack.last.children[^1] = t
        t.children.add n
        stack.add t

      else:
        parserErr "what?"

    of mtkOpenPar:
      case stack.last.kind
      of mnkVar: # is a function call
        discard

      of mnkInfix, mnkPrefix, mnkPar:
        let t = newPar()
        stack.last.children.add t
        stack.add t

      else:
        parserErr "invalid token: " & $tk

    of mtkClosePar:
      let sub = goUp(stack, (mn: MathNode) => isOpenPar(mn))

      assert sub != nil
      assert stack.len != 1

      # case stack.last.kind:
      # of mnkPar: discard
      # of mnkCall: discard
      # else: assert false

      stack.last.isFinal = true

    of mtkComma:
      discard


    when defined emathDebug:
      debugEcho ">> ", tk
      debugEcho "stack: ", stack.map(recap).join ", "
      debugEcho "tree:"
      debugEcho treeRepr stack[0]
      debugEcho "---------------------"

  stack[0].inside
