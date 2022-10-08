func isEmpty*(s: seq): bool {.inline.} =
  s.len == 0

template last*(s: seq): untyped =
  s[^1]
