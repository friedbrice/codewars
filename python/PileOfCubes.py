def find_nb(m):
  n = 0
  s = 0

  while s < m:
    n += 1
    s += n ** 3

  if s == m:
    return n
  else:
    return -1
