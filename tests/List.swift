enum List<T>
{
  case Vide
  indirect case Cons(head:T,tail:List)
  
}

enum Unit { case Unit}

enum ListError: Error { case EmptyList
                        case OutOfList
                        case LengthOfLists
}

func length<T> (_ list : List<T>) -> Int {

  switch list {
    case List.Vide : return 0
    case List.Cons(_,let tail) : 
                          return (1 + length(tail))
  
  }
}

func cons<T> (_ element : T, _ list : List<T>) -> List<T>{
  return List.Cons(head:element,tail:list)
}

func hd<T> (_ list : List<T>) throws -> T {
  switch list {
    case List.Cons(let hd,_) : return hd
    default : throw ListError.EmptyList
  }
}

func tl<T> (_ list : List<T>) throws -> List<T> {
  switch list {
    case List.Cons(_,let tl) : return tl
    default : throw ListError.EmptyList
  }
}

func nth_aux<T> (_ l : List<T>, _ n : Int) throws -> T {
    switch l {
      case List.Vide : throw ListError.OutOfList
      case List.Cons (let hd, let tl) : if n == 0 {return hd} else {return try nth_aux (tl,(n-1))}
    }
}

func nth<T> (_ list : List<T>, _ n : Int) throws -> T {
  if n<0 { throw ListError.OutOfList }
  else {
     return try nth_aux (list,n)
  }
}

func rev_append<T> (_ list1 : List<T>, _ list2 : List<T>) -> List<T> {
  switch list1 {
    case List.Vide : return list2
    case List.Cons(let hd, let tl) : return rev_append (tl,cons(hd,list2))
  }
}

func rev<T> (_ list : List<T>) -> List<T> {
  return rev_append(list,List.Vide)
}

func append<T> (_ list1 : List<T>, _ list2 : List<T>) -> List<T> {
  switch list1 {
    case List.Vide : return list2
    case List.Cons(let hd, let tl) : return cons(hd,append(tl,list2)) 
  }
}

func flatten<T> (_ list : List<List<T>>) -> List<T> {
  switch list {
    case List.Vide : return List.Vide
    case List.Cons(let hd, let tl) : return append(hd,flatten(tl))
  }
}

func concat<T> (_ list : List<List<T>>) -> List<T> {
  switch list {
    case List.Vide : return List.Vide
    case List.Cons(let hd, let tl) : return append(hd,flatten(tl))
  }
}

func map<T1,T2> (_ f : (T1) -> T2, _ list : List<T1>) -> List<T2> {
   switch list {
    case List.Vide : return List.Vide
    case List.Cons(let hd, let tl) : let r = f(hd)
                                     return cons(r,map(f,tl))
  }
}

func mapi_aux<T1,T2> (_ i : Int, _ f : (Int,T1) -> T2, _ list : List<T1>) -> List<T2> {
   switch list {
    case List.Vide : return List.Vide
    case List.Cons(let hd, let tl) : let r = f(i,hd)
                                     return cons(r,mapi_aux((i+1),f,tl))
  }
}

func mapi<T1,T2> (_ f : (Int,T1) -> T2, _ list : List<T1>) -> List<T2> {
  return mapi_aux(0,f,list)
}

func rev_map<T1,T2> (_ f : (T1) -> T2, _ list : List<T1>) -> List<T2> {
  return rev(map(f,list))
}

func fold_left<T1,T2> (_ f : (T1,T2) -> T1, _ accu : T1, _ list : List<T2>) -> T1 {
  switch list {
    case List.Vide : return accu
    case List.Cons(let hd, let tl) : return fold_left (f,f(accu,hd),tl)
  }
}

func fold_right<T1,T2> (_ f : (T1,T2) -> T2, _ list : List<T1>, _ accu : T2) -> T2 {
  switch list {
    case List.Vide : return accu
    case List.Cons(let hd, let tl) : return f(hd,fold_right(f,tl,accu))
  }
}

func map2<T1,T2,T3> (_ f : (T1,T2) -> T3,_ l1 : List<T1>, _ l2 : List<T2>) throws -> List<T3> {
  switch (l1, l2) {
    case (List.Vide, List.Vide) : return List.Vide
    case (List.Cons(let hd1,let tl1),
          List.Cons(let hd2, let tl2)) : let r = f(hd1,hd2)
                                        return cons(r, try map2(f,tl1,tl2))
    default : throw ListError.LengthOfLists
  }
}

func iter<T> (_ f : (T) -> (), _ list : List<T>) {
  switch list {
    case List.Vide : return
    case List.Cons(let hd, let tl) : f(hd);
                                    return iter(f,tl)
  }
}

func iteri_aux<T> (_ i : Int, _ f : (Int,T) -> (), _ list : List<T>) {
   switch list {
    case List.Vide : return 
    case List.Cons(let hd, let tl) : f(i,hd)
                                     return iteri_aux((i+1),f,tl)
  }
}

func iteri<T> (_ f : (Int,T) -> (), _ list : List<T>) {
  return iteri_aux(0,f,list)
}

func iter2<T1,T2> (_ f : (T1,T2) -> (), _ l1 : List<T1>, _ l2 : List<T2>) throws {
  switch (l1, l2) {
    case (List.Vide, List.Vide) : return
    case (List.Cons(let hd1,let tl1),
          List.Cons(let hd2, let tl2)) : f(hd1,hd2)
                                         return try iter2(f,tl1,tl2)
    default : throw ListError.LengthOfLists
  }
}

func Myprint<T> (_ x : T) {
  print(x)
}