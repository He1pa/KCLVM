import abc # unable to import
import .a
import .a # reimport
import .c  # test unused import for mixin name
import .d # test unused import for parent name
import .e # test unused import for union type
import .f as g # test unused import for union type

schema Main(d.Parent):
    mixin [c.TestOfMixin]
    age?: int = 18
    person?: a.Person
    list_union_type: [e.UnionType|int]
    dict_union_type: {g.UnionType|int:float}

# todo: import position check 
import .b  

if a._a > 1:
    _c = 1
elif a._a == 1:
    _c = 2
else:
    _c = 3

p = Main{
    age = a._a
}
