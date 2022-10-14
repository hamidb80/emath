## ``emath`` is a math parser/evaluator library.
## it first converts raw expression [which is given as ``string``] into AST and then evaluates it.
## 
## ``emath`` allows you to manipulate the generated AST, so it would not limit your POWER as a Nim programmer 👑.
## 
## Here's the flow:
## 
##    string   ──parsing──►   AST   ──evaluating──►   number


runnableExamples:
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
import emath/[model, defaults, exceptions]
import emath/private/utils


func `$`*(mn: MathNode): string =
  case mn.kind
  of mnkLit: $mn.value
  of mnkPar: '(' & $mn.inside & ')'
  of mnkVar: mn.ident
  of mnkCall: mn.ident & '(' & mn.children.map(`$`).join(", ") & ')'
  of mnkPrefix: $mn.operator & $mn.inside
  of mnkInfix: $mn.left & ' ' & $mn.operator & ' ' & $mn.right

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
      of mnkCall: mn.children.len >= 0
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
  of mnkPar: rec mn.inside
  of mnkVar:
    try: varLookup[mn.ident]
    except KeyError: raise undefinedErr(mn.ident, mskVar)

  of mnkCall:
    let fn =
      try: fnLookup[mn.ident]
      except KeyError: raise undefinedErr(mn.ident, mskFunc)

    fn(mn.children.mapit(rec it))

  of mnkPrefix:
    let v = rec mn.inside
    case mn.operator
    of mokPlus: v
    of mokminus: -v
    else: evalErr "invalid prefix: " & $v

  of mnkInfix:
    let
      le = rec mn.left
      ri = rec mn.right

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

template mtoken(k: MathTokenKind, i: Slice[int]): untyped =
  MathToken(kind: k, slice: i)

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

    template switch: untyped =
      case state
      of mlsFloat, mlsInt:
        yield MathToken(kind: mtkNumber, number: parseFloat input[anchor ..< i],
            slice: anchor ..< i)

      of mlsIdent:
        yield MathToken(kind: mtkIdent, ident: input[anchor ..< i],
            slice: anchor ..< i)

      of mlsOperator:
        yield MathToken(kind: mtkOperator,
          operator: parseEnum[MathOperator](input[anchor ..< i]),
          slice: anchor ..< i)

      else: raise parseErr("invalid state")

      state = mlsInitial
      continue

    template onReady(body): untyped =
      case state
      of mlsInitial: body
      else: switch

    case ch
    of Whitespace, EoS:
      case state
      of mlsInitial: discard
      else: switch

    of Operators:
      case state
      of mlsOperator:
        if ch notin {'<', '>', '='}: switch
        else: discard

      of mlsInitial: enterState mlsOperator
      else: switch

    of Digits:
      case state
      of mlsInitial: enterState mlsInt
      of mlsInt, mlsFloat, mlsIdent: discard
      else: switch

    of Letters:
      case state
      of mlsIdent: discard
      of mlsInitial: enterState mlsIdent
      else: switch

    of '.':
      case state
      of mlsInt: state = mlsFloat
      else: raise parseTokErr("hit . in wrong place", i..i)

    of ',':
      onReady:
        yield mtoken(mtkComma, i..i)

    of '(':
      onReady:
        yield mtoken(mtkOpenPar, i..i)

    of ')':
      onReady:
        yield mtoken(mtkClosePar, i..i)

    else:
      raise parseTokErr("invalid character: " & ch, i..i)


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
  var
    stack: seq[MathNode] = @[newPar()]
    lastToken: MathToken

  for tk in lex input:
    case tk.kind
    of mtkNumber, mtkIdent:
      let t =
        if tk.kind == mtkNumber: newLiteral tk.number
        else: newVar tk.ident

      stack.last.children.add t
      stack.add t

    of mtkOperator:
      if
        (stack.last.kind == mnkPar) and (stack.last.children.len == 0) or
        (stack.last.kind in {mnkInfix, mnkprefix}) or
        ((stack.last.kind == mnkCall) and not stack.last.isFinal):

        case tk.operator
        of mokPlus, mokminus:
          let t = newPrefix tk.operator
          stack.last.children.add t
          stack.add t

        else:
          raise parseTokErr("invalid prefix operator " & $tk.operator, tk.slice)

      elif stack.last.kind in {mnkLit, mnkVar} or
          stack.last.kind in {mnkPar, mnkCall} and stack.last.isFinal:

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
        raise parseTokErr("hit operator " & $tk.operator &
            " in unexpected place", tk.slice)

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
        raise parseTokErr("hit '(' in unexpected place", tk.slice)

    of mtkClosePar:
      discard goUp(stack, (mn: MathNode) => isOpenWrapper(mn))

      if stack.len == 1:
        raise parseTokErr("hit ')' in unexpected place", tk.slice)

      elif stack.last.kind == mnkPar and stack.last.children.len == 0:
        raise parseTokErr("parenthesis must have 1 subnode, given 0", tk.slice)

      stack.last.isFinal = true

    of mtkComma:
      discard goUp(stack, (mn: MathNode) => isOpenWrapper(mn))

      if stack.last.kind != mnkCall or lastToken.kind in {mtkComma, mtkOpenPar, mtkOperator}:
        raise parseTokErr("hit ',' in unexpected place", tk.slice)

    lastToken = tk
    when defined emathDebug:
      debugEcho ">> ", tk
      debugEcho "stack: ", stack.map(recap).join ", "
      debugEcho "tree:"
      debugEcho treeRepr stack[0]
      debugEcho "---------------------"


  result = stack.first.inside

  if not isValid result:
    raise parseErr "the expression is incomplete, either closing parenthesis are not enough or there are some infixes without right side"
