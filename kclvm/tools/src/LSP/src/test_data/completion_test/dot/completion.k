import .pkg.  # complete import path
import .pkg.subpkg
schema Person:
    name: str
    age: int

p = Person {
    name: "alice"
    age: 1
}

p1 = p.       # complete schema attr   p : Type None

p2 = p.name.  # complete builtin (str) function

p3 = subpkg.  # complete user module definition   subpg: Type None

import math
math.         # complete system module definition   math; ->subpkg

"a".    # "a" -> sub pkg

p4 = Person{
    
}

schema P:
    a: int = 1

aaaa = P{}.
