import std/[tables, strutils, sequtils, math]
import emath/[model, utils]


func `$`*(mn: MathNode): string =
  case mn.kind
  of mnkLit: $mn.value
  of mnkPar: '(' & $mn.children[0] & ')'
  of mnkVar: mn.ident
  of mnkCall: mn.ident & '(' & mn.children.map(`$`).join(", ") & ')'
  of mnkPrefix: $mn.operator & $mn.children[0]
  of mnkInfix: $mn.children[0] & ' ' & $mn.operator & ' ' & $mn.children[1]

func recap(mn: MathNode): string =
  case mn.kind
  of mnkLit: "LIT " & $mn.value
  of mnkPar: "PAR"
  of mnkVar: "IDENT " & mn.ident
  of mnkCall: "CALL " & mn.ident
  of mnkPrefix: "PREFIX " & $mn.operator
  of mnkInfix: "INFIX " & $mn.operator


func treeReprImpl(mn: MathNode, result: var seq[string], level: int,
    tab = 2) =

  template incl(smth, lvl): untyped =
    result.add indent(smth, lvl * tab)

  template incl(smth): untyped =
    incl smth, level

  template inclChildren(children): untyped =
    for ch in children:
      treeReprImpl ch, result, level + 1

  case mn.kind
  of mnkLit:
    incl "LIT " & $mn.value

  of mnkPrefix:
    incl "PREFIX " & $mn.operator
    inclChildren mn.children

  of mnkInfix:
    incl "INFIX " & $mn.operator
    inclChildren mn.children

  # of mnkPar: "PAR" ... $mn.children[0]
  # of mnkVar: "VAR " & mn.ident
  # of mnkCall: mn.ident & '(' & mn.children.map(`$`).join(", ") & ')'
  else: discard

func treeRepr*(mn: MathNode): string =
  ## for debugging purposes
  var acc: seq[string]
  treeReprImpl mn, acc, 0

  acc.join "\n"


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
  var
    v: MathVarLookup
    f: MathFnLookup

  eval mn, v, f


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

# FIXME apply "code graph" from "Grokking simplicity"

func parse*(input: string): MathNode =
  var stack: seq[MathNode]

  for tk in lex input:
    case tk.kind
    of mtkNumber:
      let t = toMathNode tk.number

      if not isEmpty stack:
        case stack.last.kind
        of mnkInfix, mnkPrefix:
          stack.last.children.add t

        else:
          parserErr "the last is: " & $stack.last.kind

      stack.add t

    of mtkOperator:
      if isEmpty stack:
        stack.add MathNode(kind: mnkPrefix, operator: tk.operator)

      elif stack.last.kind == mnkprefix:
        let t = MathNode(kind: mnkPrefix, operator: tk.operator)
        stack.last.children.add t
        stack.add t

      elif stack.last.kind == mnkInfix:
        debugEcho "??"
        var t = MathNode(kind: mnkPrefix, operator: tk.operator)
        stack.last.children.add t
        stack.add t

      elif stack.len == 1:
        debugEcho ">>"

        var
          t = MathNode(kind: mnkInfix, operator: tk.operator)
          n = stack.pop

        t.children.add n
        stack.add t

      elif stack[^2].kind == mnkInfix:
        var
          temp = MathNode(kind: mnkInfix, operator: tk.operator)
          n = stack.pop
          l = n

        while stack.len > 0:
          if tk.operator.priority > stack.last.operator.priority:
            temp.children.add stack.last.children[1]
            stack.last.children[1] = temp
            shoot stack
            stack.add temp
            break

          else:
            l = stack.pop

      elif stack[^2].kind == mnkPrefix:
        var t = MathNode(kind: mnkInfix, operator: tk.operator)

        if t.operator.priority > stack[^2].operator.priority: # -1 * 2
          let n = stack.pop
          t.children.add n
          stack[^1].children = @[t]
          stack.add t

        else: # -1 - 2
          debugEcho "!!"

          while stack.len > 1:
            shoot stack

            if stack.last.kind == mnkInfix:
              break

          t.children.add stack.pop
          stack.add t


    of mtkIdent: discard
    of mtkOpenPar: discard
    of mtkClosePar: discard
    of mtkComma: discard


    when defined emathDebug:
      debugEcho ">> ", tk
      debugEcho "stack: ", stack.map(recap).join ", "
      debugEcho "tree:"
      debugEcho treeRepr stack[0]
      debugEcho "---------------------"

  stack[0]
