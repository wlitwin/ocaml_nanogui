type t = {a:float; b:float}
let zero = {a=0.; b=0.}

let mk a b = {a; b}

let mk1 a = {a; b=a}

let ( + ) v1 v2 = {
    a = v1.a +. v2.a;
    b = v1.b +. v2.b;
}

let ( - ) v1 v2 = {
    a = v1.a -. v2.a;
    b = v1.b -. v2.b;
}

let ( * ) v1 s = {
    a = v1.a *. s;
    b = v1.b *. s;
}

let length v = 
    Float.(sqrt (v.a*v.a + v.b*v.b))

let max v1 v2 = {
    a = Float.max v1.a v2.a;
    b = Float.max v1.b v2.b;
}

let min v1 v2 = {
    a = Float.min v1.a v2.a;
    b = Float.min v1.b v2.b;
}

let inv (v : t) = {
    a = ~-.(v.a);
    b = ~-.(v.b);
}

let inv_x (v : t) = {
    v with a = ~-.(v.a);
}

let inv_y (v : t) = {
    v with b = ~-.(v.b);
}

let set_a v a = {v with a}
let set_b v b = {v with b}
let get_a v = v.a
let get_b v = v.b
