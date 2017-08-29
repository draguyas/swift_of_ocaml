func f (_ x : Int ) -> Int {
return (x + 3)
}

func g (_ x : Int ) -> Int {
if true {
 return x
}else{
 return (x - 1)
}
}

func concat_string (_ s1 : String, _ s2 : String ) -> String {
return (s1 + s2)
}

func simple_match (_ x : Int ) -> Int {
switch x {
case 0 : return x
case 1 : return (x - 1)
default : return (x + 2)
}
}

func match_if (_ x : Int ) -> Int {
switch x {
case 0 : if ((x + 1) < 5){
 return x
}else{
 return 3}

default : return x
}
}


