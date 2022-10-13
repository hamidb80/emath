## ``emath`` is a math parser/evaluator library.
## it first converts raw expression [which is given as ``string``] into AST and then evaluates it.
## 
## ``emath`` allows you to manipulate the generated AST, so it would not limit your POWER as a Nim programmer 👑.
## 
## Here's the flow:
## 
##    string   ──parsing──►   AST   ──evaluating──►   number


runnableExamples:
  import emath

  # evaluating with default functions and variables
  echo "1 + sin(PI)".parse.eval # 1.0

  # using custom variables and functions
  import std/tables
  import emath/defaults

  let vars = toTable {
    "myvar": 6.6
  }

  var fns = defaultFns
  fns["pow2"] = proc(args: seq[float]): float =
    args[0] * args[0]

  let ans = "myvar * pow2(3)".parse.eval(vars, fns)
  echo ans # 59.4


import std/[tables, strutils, sequtils, math, sugar]
import emath/[model, defaults]
import emath/private/utils


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

  for ch in mn.children:
    treeReprImpl ch, result, level + 1

func treeRepr*(mn: MathNode): string =
  ## converts a `MathNode` into its corresponding tree representation.
  ##
  ## can be used for debugging purposes.
  var acc: seq[string]
  treeReprImpl mn, acc, 0
  acc.join "\n"


func isValid*(mn: MathNode): bool =
  ## check for any AST errors in genereated AST
  let
    numberOfChildren =
      case mn.kind
      of mnkLit, mnkVar: true
      of mnkCall: mn.children.len > 0
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

func eval*(mn: MathNode,
  varLookup: MathVarLookup,
  fnLookup: MathFnLookup): float =
  ## calculates the final answer

  template rec(n): untyped =
    eval(n, varLookup, fnLookup)

  case mn.kind
  of mnkLit: mn.value
  of mnkPar: rec mn.children[0]
  of mnkVar:
    try: varLookup[mn.ident]
    except KeyError: undefinedErr(mn.ident, mikVar)

  of mnkCall:
    let fn =
      try: fnLookup[mn.ident]
      except KeyError: undefinedErr(mn.ident, mikFunc)

    fn(mn.children.mapit(rec it))

  of mnkPrefix:
    let v = rec mn.children[0]
    case mn.operator
    of mokPlus: v
    of mokminus: -v
    else: evalErr "invalid prefix: " & $v

  of mnkInfix:
    let
      le = rec mn.children[0]
      ri = rec mn.children[1]

    case mn.operator
    of mokPow: pow(le, ri)
    of mokMult: le * ri
    of mokDiv: le / ri
    of mokPlus: le + ri
    of mokminus: le - ri
    of mokMod: floorMod(le, ri)
    of mokLarger: float le > ri
    of mokLargerEq: float le >= ri
    of mokEq: float le == ri
    of mokAlmostEq: float almostEqual(le, ri)
    of mokLessEq: float le <= ri
    of mokLess: float le < ri

func eval*(mn: MathNode): float =
  ## calculates the final answer with default variables and default functions
  eval mn, defaultVars, defaultFns


const
  Operators = {'+', '-', '*', '/', '^', '~', '=', '<', '>', '%'}
  EoS = '\0' # End of String

type MathLexerState = enum
  mlsInitial
  mlsInt, mlsFloat
  mlsOperator
  mlsIdent

iterator lex(input: string): MathToken =
  var
    i = 0
    anchor = 0
    state = mlsInitial

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

      else: lexErr "invalid state"

      state = mlsInitial
      continue

    template onReady(body): untyped =
      case state
      of mlsInitial: body
      else: twist

    case ch
    of Whitespace, EoS:
      case state
      of mlsInitial: discard
      else: twist

    of Operators:
      case state
      of mlsOperator:
        if ch notin {'<', '>', '='}: twist
        else: discard

      of mlsInitial: enterState mlsOperator
      else: twist

    of Digits:
      case state
      of mlsInitial: enterState mlsInt
      of mlsInt, mlsFloat, mlsIdent: discard
      else: twist

    of Letters:
      case state
      of mlsIdent: discard
      of mlsInitial: enterState mlsIdent
      else: twist

    of '.':
      case state
      of mlsInt: state = mlsFloat
      else: lexErr "hit . in wrong place"

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
      lexErr "invalid character: " & ch


    inc i


func isOpenWrapper(mn: MathNode): bool =
  (mn.kind in {mnkPar, mnkCall}) and (not mn.isFinal)

func goUp(stack: var seq[MathNode], fn: MathNode -> bool): MathNode =
  ## goes up of a sun tree until satisfies `fn`
  ## returns sub tree, could be nil
  while true:
    if (fn stack.last): return
    else: result = stack.pop

  raise newException(ValueError, "couldn't find the desired node")

func parse*(input: string): MathNode =
  ## parses the math expression from raw string into its corresponding AST
  var stack: seq[MathNode] = @[newPar()]

  for tk in lex input:
    case tk.kind
    of mtkNumber, mtkIdent:
      let t =
        if tk.kind == mtkNumber: newLiteral tk.number
        else: newVar tk.ident

      assert stack.last.kind in {mnkInfix, mnkPrefix, mnkPar, mnkCall}
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
            isOpenWrapper(mn) or
            (mn.kind in {mnkInfix, mnkPrefix}) and
            (p > mn.operator.priority))

        stack.last.children[^1] = t
        t.children.add n
        stack.add t

      else:
        parseErr "operator" & $tk.operator & " in unexpected place"

    of mtkOpenPar:
      case stack.last.kind
      of mnkVar: # is a function call
        let t = newCall(stack.pop.ident)
        stack.last.children[^1] = t
        stack.add t

      of mnkInfix, mnkPrefix, mnkPar:
        let t = newPar()
        stack.last.children.add t
        stack.add t

      else:
        parseErr "hit '(' in unexpected place"

    of mtkClosePar:
      let sub = goUp(stack, (mn: MathNode) => isOpenWrapper(mn))
      assert sub != nil

      if stack.last.kind == mnkPar and stack.last.children.len == 0:
        parseErr "parenthesis must have 1 subnode, given 0"

      stack.last.isFinal = true

    of mtkComma:
      discard goUp(stack, (mn: MathNode) => isOpenWrapper(mn))

      if stack.last.kind != mnkCall:
        parseErr "hit ',' in unexpected place"


    when defined emathDebug:
      debugEcho ">> ", tk
      debugEcho "stack: ", stack.map(recap).join ", "
      debugEcho "tree:"
      debugEcho treeRepr stack[0]
      debugEcho "---------------------"

  stack.first.inside
