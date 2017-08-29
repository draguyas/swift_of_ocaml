func f (_ l : List<Int> ) -> List<Int> {
switch l {
case List.Vide : return List.Vide
case List.Cons(let e,let l) : return cons((e + 3), f(l))
}
}


