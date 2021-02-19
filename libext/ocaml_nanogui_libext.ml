include Stdlib

module Float = struct
    include Float

    let ( + ) = add
    let ( - ) = sub
    let ( / ) = div
    let ( * ) = mul

    let ( ~- ) = neg

    let round_down = floor

    let lerp a b ~t =
        a + t*(b - a)

    let clamp_exn v ~min ~max =
        if v < min then min
        else if v > max then max
        else v
end

module List = struct
    include List

    let mem lst v =
        mem v lst

    let filter lst ~f =
        filter f lst

    let iteri lst ~f =
        iteri f lst
end

module Char = struct
    include Char

    let of_int i =
        try Some (chr i)
        with _ -> None

    let to_string c =
        String.make 1 c
end

module Int = struct
    include Int
    
    let ( + ) = add
    let ( - ) = sub
    let ( / ) = div
    let ( * ) = mul

    let clamp ~min ~max v =
        if v < min then min
        else if v > max then max
        else v

    let min a b =
        if a < b then a else b
end

module String = struct
    include String

    let is_empty s = length s = 0

    let suffix s len =
        let start = max (length s - len) 0 in
        sub s start len

    let prefix s len =
        let len = min (length s) len in
        sub s 0 len

    let slice s start end_ =
        let max = length s - 1 in
        let end_ = if end_ = 0 then length s else end_ in
        sub s start (min (end_ - start) max)

    let sub s ~pos ~len =
        sub s pos len
end

module Option = struct
    let is_some = function
        | Some _ -> true
        | None -> false

    let value_exn = function
        | None -> failwith "Expected some"
        | Some v -> v

    module Let_syntax = struct
        let ( >>= ) x f =
            match x with
            | None -> None
            | Some x -> f x

        let ( >>| ) x f =
            match x with
            | None -> None
            | Some x -> Some (f x)
    end

    include Let_syntax
end

