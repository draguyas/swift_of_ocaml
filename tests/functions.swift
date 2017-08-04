func f (_ x : Int, _ y : Int ) -> Int {
return ((x * 4) + y)
}

func g (_ y : Int ) -> Int {
return (y * 4)
}

(f(3,2) + g(3))
