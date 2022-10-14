import std/[math, tables, macros]
import model


macro toEMathFn(fnIdent: untyped, argsLen: static[int]): untyped =
  let
    fnStr = fnIdent.strval
    call = newCall(fnIdent)
    argsId = ident"args"
    returnType = quote: seq[float]
    argsLenCheck = quote:
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
  defaultFns*: EMathFnLookup = block:
    var res: EMathFnLookup
    res["sin"] = toEMathFn(sin, 1)
    res["cos"] = toEMathFn(cos, 1)
    res["tan"] = toEMathFn(tan, 1)
    res["cot"] = toEMathFn(cot, 1)
    res["sec"] = toEMathFn(sec, 1)
    res["csc"] = toEMathFn(csc, 1)
    res["sinh"] = toEMathFn(sinh, 1)
    res["cosh"] = toEMathFn(cosh, 1)
    res["tanh"] = toEMathFn(tanh, 1)
    res["coth"] = toEMathFn(coth, 1)
    res["sech"] = toEMathFn(sech, 1)
    res["csch"] = toEMathFn(csch, 1)
    res["arcsin"] = toEMathFn(arcsin, 1)
    res["arccos"] = toEMathFn(arccos, 1)
    res["arctan"] = toEMathFn(arctan, 1)
    res["arccot"] = toEMathFn(arccot, 1)
    res["arcsec"] = toEMathFn(arcsec, 1)
    res["arccsc"] = toEMathFn(arccsc, 1)
    res["arcsinh"] = toEMathFn(arcsinh, 1)
    res["arccosh"] = toEMathFn(arccosh, 1)
    res["arctanh"] = toEMathFn(arctanh, 1)
    res["arccoth"] = toEMathFn(arccoth, 1)
    res["arcsech"] = toEMathFn(arcsech, 1)
    res["arccsch"] = toEMathFn(arccsch, 1)
    res["log"] = toEMathFn(log, 2)
    res["log2"] = toEMathFn(log2, 1)
    res["lg"] = toEMathFn(log2, 1)
    res["ln"] = toEMathFn(ln, 1)
    res["log10"] = toEMathFn(log10, 1)
    res["sqrt"] = toEMathFn(sqrt, 1)
    res

  defaultVars*: EMathVarLookup = toTable {
    "PI": PI,
    "TAU": TAU,
    "e": E
  }
