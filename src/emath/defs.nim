import std/[math, tables, macros]
import model


macro toEmathFn(fnIdent: untyped, argsLen: static[int]): untyped =
  let
    fnStr = fnIdent.strval
    call = newCall(fnIdent)
    argsId = ident"args"
    returnType = quote: seq[float]

  let argsLenCheck = quote:
    doAssert `argsId`.len == `argsLen`:
      "invalid number of arguments for function " & `fnStr` &
      ". expected " & $`argsLen` & " but given " & $`argsId`.len

  for n in 1..argsLen:
    call.add newTree(nnkBracketExpr, argsId, newlit n-1)

  newProc(
    params = @[ident"float", newIdentDefs(argsId, returnType)],
    procType = nnkFuncDef,
    body = newStmtList(argsLenCheck, call))


const
  defaultFns*: MathFnLookup = toTable {
    "sin": toEmathFn(sin, 1),
    "cos": toEmathFn(cos, 1),
    "tan": toEmathFn(tan, 1),
    "cot": toEmathFn(cot, 1),
    "sec": toEmathFn(sec, 1),
    "csc": toEmathFn(csc, 1),

    "sinh": toEmathFn(sinh, 1),
    "cosh": toEmathFn(cosh, 1),
    "tanh": toEmathFn(tanh, 1),
    "coth": toEmathFn(coth, 1),
    "sech": toEmathFn(sech, 1),
    "csch": toEmathFn(csch, 1),

    "arcsin": toEmathFn(arcsin, 1),
    "arccos": toEmathFn(arccos, 1),
    "arctan": toEmathFn(arctan, 1),
    "arccot": toEmathFn(arccot, 1),
    "arcsec": toEmathFn(arcsec, 1),
    "arccsc": toEmathFn(arccsc, 1),
    "arcsinh": toEmathFn(arcsinh, 1),
    "arccosh": toEmathFn(arccosh, 1),
    "arctanh": toEmathFn(arctanh, 1),
    "arccoth": toEmathFn(arccoth, 1),
    "arcsech": toEmathFn(arcsech, 1),
    "arccsch": toEmathFn(arccsch, 1),

    "log": toEmathFn(log, 2),
    "log2": toEmathFn(log2, 1),
    "lg": toEmathFn(log2, 1),
    "ln": toEmathFn(ln, 1),
    "log10": toEmathFn(log10, 1),

    "sqrt": toEmathFn(sqrt, 1),
  }

  defaultVars*: MathVarLookup = toTable {
    "PI": PI,
    "TAU": TAU,
    "e": E
  }
