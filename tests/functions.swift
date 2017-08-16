func f (_ x : Float ) -> Float {
switch x {
case 0.0 : return 0.0
case 1.0 : if ((x - 1.0) == 0.0){
 return 0.0
}else{
 return 1.0}

default : return 42.0
}
}

func g (_ x : Float ) -> String {
if (f(x) == 42.0) {
 return "life"
}else{
 return "noob"
}
}


