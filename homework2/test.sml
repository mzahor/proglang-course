datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

fun max_constant e =
 case e of
     Constant i => i
   | Negate i => max_constant i
   | Add (a, b) => Int.max(max_constant(a), max_constant(b)) 
   | Multiply (a, b) => Int.max(max_constant(a), max_constant(b)) 

val test_exp = Add(Constant 19, Negate (Constant 4))
val nineteen = max_constant test_exp
