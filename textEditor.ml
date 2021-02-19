open Batteries

type t = {
    rope : Text.t;
    cursor_abs : int;
}

let empty = {
    rope = Text.empty;
    cursor_abs = 0;
}

let insert t c =
    let rope = Text.insert t.cursor_abs (Text.of_char c) t.rope in
    {
        rope; 
        cursor_abs = t.cursor_abs + 1;
    }
;;

let move_cursor_left t =
    if t.cursor_abs > 0 then {
        t with cursor_abs = t.cursor_abs - 1;
    } else t
;;

let delete_at_cursor (t : t) =
    if Text.length t.rope > 0 && t.cursor_abs > 0 then {
        rope = Text.remove (t.cursor_abs-1) 1 t.rope;
        cursor_abs = t.cursor_abs - 1;
    } else t

let move_cursor_right t =
    if t.cursor_abs < Text.length t.rope then {
        t with cursor_abs = t.cursor_abs + 1;
    } else t
;;

let move_cursor_up t =
    (* Look backwards for a '\n' *)
    let newline = UChar.of_char '\n' in
    let cursor_abs = t.cursor_abs in
    let rindex_from pos =
        try Text.rindex_from t.rope pos newline with Caml.Not_found -> -1
    in
    let last_line = rindex_from (cursor_abs - 1) in
    if last_line < 0 then t
    else (
        let last_last_line = rindex_from (last_line - 1) in
        (* New index is min(last_last_line + (cursor_abs - last_line), last_line) *)
        Printf.printf "LL %d LLL %d C %d\n" last_line last_last_line cursor_abs;
        { t with cursor_abs = min last_line (last_last_line + (cursor_abs - last_line)) }
    )
;;

let move_cursor_down t =
    (* Look backwards for a '\n' *)
    let newline = UChar.of_char '\n' in
    let cursor_abs = t.cursor_abs in
    let index_from pos = 
        try 
            Text.index_from t.rope pos newline 
        with Caml.Not_found | Text.Out_of_bounds -> Text.length t.rope 
    in
    let rindex_from pos = try Text.rindex_from t.rope pos newline with Caml.Not_found -> -1 in
    let index = cursor_abs - rindex_from (cursor_abs - 1) in
    let next_line = index_from cursor_abs in
    let next_next_line = index_from (next_line + 1) in
    Printf.printf "NL %d NLL %d C %d\n" next_line next_next_line cursor_abs;
    if next_line = Text.length t.rope then t
    else { t with cursor_abs = min next_next_line (next_line + index) }
;;

(* TODO - up/down *)

let iter t f =
    Text.iter f t

let longest_line t =
    let max, _, lines =
        Text.fold (fun (max_len, cur_len, lines) c ->
            match UChar.char_of c with
            | '\n' -> (max max_len cur_len, 0, lines+1)
            | _ -> 
                let cur_len = cur_len + 1 in
                (max max_len cur_len, cur_len, lines)
        ) (0, 0, 1) t.rope
    in
    max, lines
;;

let cursor_pos t = t.cursor_abs

let to_string t = Text.to_string t.rope
let of_string str = {
    rope = Text.of_string str;
    cursor_abs = String.length str;
}
