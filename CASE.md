Case Checking
-------------

Checking for exhaustiveness:

```
case e # Type (String, (Int, String))
  (s, (0, _)) -> ...
  (s, _)      -> ...
  _           -> ...
```
usefulness function = U(P, q), where P is a pattern matrix and q is a pattern vector
Exhaustiveness: U(pattern_matrix, (_))

```
U( | (s, (0, _)) |, | _ | )
   |   (s, _)    |
   |     _       |

q1 = _
case 2a: q1 is a wildcard and all constructors of a 2-tuple (there is only one) exist in first column of P
useful = any? { |ctor| U(S(ctor, P), S(ctor, q)) } # Only one 2-tuple ctor, so only one recurse to check

S(ctor, P)
S(2-tuple, | (s, (0, _)) |)
           |   (s, _)    |
           |     _       |

should give back matrix width of 2
= | s (0, _) |
  | s   _    |
  | _   _    |

S(ctor, q)
S(2-tuple, (_))
= | _ _ |

U( | s (0, _) |, | _ _ | )
   | s   _    |
   | _   _    |

q1 = _
case 2b: q1 is a wilcard and there are no constructors for String
useful = U(D(P), q2...qn)

D(P) = | (0, _) |
       |   _    |
       |   _    |

U( | (0, _) |, | _ | )
   |   _    |
   |   _    |

q1 = _
case 2a: q1 is a wildcard and all 2-tuple constructors exist in first column of P
useful = any? { | ctor| U(S(ctor, P), S(ctor, q)) }

S(ctor, P)
S(2-tuple, | (0, _) | )
           |   _    |
           |   _    |
= | 0 _ |
  | _ _ |
  | _ _ |

S(ctor, q)
S(2-tuple, | _ | )
= | _ _ |

U( | 0 _ |, | _ _ | )
   | _ _ |
   | _ _ |

q1 = _
case 2b: q1 is wildcard, not a complete signature of Ints (only 0 is represented)
useful = U(D(P), q2...qn)

D(P) = | _ |
       | _ |

U( | _ |, | _ | )
   | _ |

q1 = _
case 2b: q1 is wildcare, no constructors in first column of P
useful = U(D(P), q2...qn)

D(P) = | |
       | |

# U of 2x0 matrix (no columns), and 0-length vector
U( | |, | | )
   | |

U(matrix with n-rows and 0-columns, ||) = false

Therefore, the original pattern vector of (_) is useless.
This means that adding a single wildcard pattern to the patterns
in the case adds nothing of value; the patterns in the case are
already exhaustive. Don't throw an error in this case.
```

----

Checking if the 3rd pattern is useless:

```
case e # Type (String, (Int, String))
  (s, (0, _)) -> ...
  (s, _)      -> ...
  _           -> ... # Should be useless
```
usefulness function = U(P, q), where P is a pattern matrix and q is a pattern vector
Usefulness: U(P1..i-1, Pi)

```
U( | (s, (0, _)) |, | _ | )
   |   (s, _)    |

q1 = _
case 2a: q1 is wildcare, all 2-tuple constructors (only 1) represented in first column of P
useful = any? { |ctor| U(S(ctor, P), S(ctor, q)) }

S(ctor, P) = S(2-tuple, | (s, (0, _)) |
                        |   (s, _)    |

= | s (0, _) |
  | s   _    |

S(ctor, q) = S(2-tuple, |_|) = | _ _ |

U( | s (0, _) |, | _ _ | )
   | s   _    |

q1 = _
case 2b: q1 is wildcard, no String constructors
useful = U(D(P), q2..qn)

D(P) = | (0, _) |
       |   _    |

U( | (0, _) |, | _ | )
   |   _    |

q1 = _
case 2a: q1 is wildcard, 2-tuple constructor present in first column of P
useful = any? { |ctor| U(S(ctor, P), S(ctor, q)) }

S(ctor, P) = S(2-tuple, | (0, _) | )
                        |   _    |
= | 0 _ |
  | _ _ |

S(ctor, q) = S(2-tuple, | _ |)
= | _ _ |

U( | 0 _ |, | _ _ | )
   | _ _ |

q1 = _
case 2b: q1 is wildcard, only 0 represented for Ints (incomplete)
useful = U(D(P), q2..qn)

D(P) = | _ |

U( | _ |, | _ | )

q1 = _
case 2b: q1 is wildcard, no constructors
useful = U(D(P), q2..qn)

D(P) = | |

# U called with 1x0 matrix (1 row, no columns), and 0-length vector
U( | |, | |)

U(matrix with n-rows and 0-columns, ||) = false
Therefore, the 3rd pattern of _ is useless. All values would
already match a previous pattern. This should throw an error
about this redundant pattern. It has to be removed.
