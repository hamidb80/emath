func isEmpty*(s: seq): bool {.inline.} =
  s.len == 0

template last*(s: seq): untyped =
  s[^1]

func shoot*(s: var seq) =
  s.del s.high