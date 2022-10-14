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

export exceptions, model

func `$`*(mn: MathNode): string =
  case mn.kind
  of emnkLit: $mn.value
  of emnkPar: '(' & $mn.inside & ')'
  of emnkVar: mn.ident
  of emnkCall: mn.ident & '(' & mn.children.map(`$`).join(", ") & ')'
  of emnkPrefix: $mn.operator & $mn.inside
  of emnkPostfix: $mn.inside & $mn.operator
  of emnkInfix: $mn.left & ' ' & $mn.operator & ' ' & $mn.right

func recap(mn: MathNode): string {.used.} =
  case mn.kind
  of emnkLit: "LIT " & $mn.value
  of emnkPar: "PAR"
  of emnkVar: "IDENT " & mn.ident
  of emnkCall: "CALL " & mn.ident
  of emnkPrefix: "PREFIX " & $mn.operator
  of emnkPostfix: "POSTFIX " & $mn.operator
  of emnkInfix: "INFIX " & $mn.operator

func treeReprImpl(mn: MathNode, result: var seq[string],
  level: int, tab = 2) =

  template incl(smth): untyped =
    result.add indent(smth, level * tab)

  incl:
    case mn.kind
    of emnkLit: "LIT " & $mn.value
    of emnkPrefix: "PREFIX " & $mn.operator
    of emnkPostfix: "POSTFIX " & $mn.operator
    of emnkInfix: "INFIX " & $mn.operator
    of emnkPar: "PAR"
    of emnkVar: "VAR " & mn.ident
    of emnkCall: "CALL " & mn.ident

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
      of emnkLit, emnkVar: true
      of emnkCall: mn.children.len >= 0
      of emnkPar, emnkPrefix, emnkPostfix: mn.children.len == 1
      of emnkInfix: mn.children.len == 2

    subNodes =
      case mn.kind
      of emnkLit, emnkVar: true
      of emnkPar, emnkCall, emnkPostfix, emnkPrefix, emnkInfix:
        mn.children.allIt isValid it

    closed =
      case mn.kind
      of emnkPar, emnkCall: mn.isFinal
      else: true

  numberOfChildren and subNodes and closed

func eval*(mn: MathNode,
  varLookup: EMathVarLookup,
  fnLookup: EMathFnLookup): float =
  ## calculates the final answer

  template rec(n): untyped =
    eval(n, varLookup, fnLookup)

  case mn.kind
  of emnkLit: mn.value
  of emnkPar: rec mn.inside
  of emnkVar:
    try: varLookup[mn.ident]
    except KeyError: raise undefinedErr(mn.ident, mskVar)

  of emnkCall:
    let fn =
      try: fnLookup[mn.ident]
      except KeyError: raise undefinedErr(mn.ident, mskFunc)

    fn(mn.children.mapit(rec it))

  of emnkPostfix:
    let v = rec mn.inside
    case mn.operator
    of emokNotFact:
      if isInt v: float fac v.toInt
      else: evalErr "factorial only works for integers, got float " & $v
    else: evalErr "invalid postfix: " & $v

  of emnkPrefix:
    let v = rec mn.inside
    case mn.operator
    of emokPlus: v
    of emokMinus: -v
    of emokNotFact: float not v.toBinary
    else: evalErr "invalid prefix: " & $mn.operator

  of emnkInfix:
    let
      le = rec mn.left
      ri = rec mn.right

    case mn.operator
    of emokPow: pow(le, ri)
    of emokMult: le * ri
    of emokDiv: le / ri
    of emokPlus: le + ri
    of emokMinus: le - ri
    of emokMod: floorMod(le, ri)
    of emokLarger: float le > ri
    of emokLargerEq: float le >= ri
    of emokEq: float le == ri
    of emokNotEq: float le != ri
    of emokAlmostEq: float almostEqual(le, ri)
    of emokLessEq: float le <= ri
    of emokLess: float le < ri
    of emokAnd: float le.toBinary and ri.toBinary
    of emokOr: float le.toBinary or ri.toBinary
    else: evalErr "invalid infix operator " & $mn.operator

func eval*(mn: MathNode): float =
  ## calculates the final answer with default variables and default functions
  eval mn, defaultVars, defaultFns


const
  Operators = {'+', '-', '*', '/', '^', '~', '=', '<', '>', '%', '&', '|', '!'}
  EoS = '\0' # End of String

type MathLexerState = enum
  mlsInitial
  mlsInt, mlsFloat
  mlsOperator
  mlsIdent

template mtoken(k: EMathTokenKind, i: Slice[int]): untyped =
  EMathToken(kind: k, slice: i)

iterator lex(input: string): EMathToken =
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
        yield EMathToken(kind: emtkNumber, number: parseFloat input[anchor ..<
            i], slice: anchor ..< i)

      of mlsIdent:
        yield EMathToken(kind: emtkIdent, ident: input[anchor ..< i],
            slice: anchor ..< i)

      of mlsOperator:
        yield EMathToken(kind: emtkOperator,
          operator: parseEnum[EMathOperator](input[anchor ..< i]),
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
        yield mtoken(emtkComma, i..i)

    of '(':
      onReady:
        yield mtoken(emtkOpenPar, i..i)

    of ')':
      onReady:
        yield mtoken(emtkClosePar, i..i)

    else:
      raise parseTokErr("invalid character: " & ch, i..i)


    inc i


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
    lastToken: EMathToken

  for tk in lex input:
    case tk.kind
    of emtkNumber, emtkIdent:
      if isFinalValue stack.last:
        raise parseTokErr("got token of kind '" & $tk.kind & "' in wrong place", tk.slice)

      else:
        let t =
          if tk.kind == emtkNumber: newLiteral tk.number
          else: newVar tk.ident

        stack.last.children.add t
        stack.add t

    of emtkOperator:
      if isFinalValue stack.last:

        if tk.operator == emokNotFact:
          let t = newPostfix(tk.operator, stack.pop)
          stack.last.children[^1] = t
          stack.add t

        else:
          var
            t = newInfix tk.operator
            p = t.operator.priority
            n = goUp(stack, (mn: MathNode) =>
              isOpenWrapper(mn) or
              (mn.kind in {emnkInfix, emnkPrefix}) and
              (p > mn.operator.priority))

          stack.last.children[^1] = t
          t.children.add n
          stack.add t

      else:
        case tk.operator
        of emokPlus, emokMinus, emokNotFact:
          let t = newPrefix tk.operator
          stack.last.children.add t
          stack.add t

        else:
          raise parseTokErr("invalid prefix operator " & $tk.operator, tk.slice)

    of emtkOpenPar:
      case stack.last.kind
      of emnkVar: # is a function call
        let t = newCall(stack.pop.ident)
        stack.last.children[^1] = t
        stack.add t

      of emnkInfix, emnkPrefix, emnkPar:
        let t = newPar()
        stack.last.children.add t
        stack.add t

      else:
        raise parseTokErr("hit '(' in unexpected place", tk.slice)

    of emtkClosePar:
      discard goUp(stack, (mn: MathNode) => isOpenWrapper(mn))

      if stack.len == 1:
        raise parseTokErr("hit ')' in unexpected place", tk.slice)

      elif stack.last.kind == emnkPar and stack.last.children.len == 0:
        raise parseTokErr("parenthesis must have 1 subnode, given 0", tk.slice)

      stack.last.isFinal = true

    of emtkComma:
      discard goUp(stack, (mn: MathNode) => isOpenWrapper(mn))

      if stack.last.kind != emnkCall or lastToken.kind in {emtkComma,
          emtkOpenPar, emtkOperator}:
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
