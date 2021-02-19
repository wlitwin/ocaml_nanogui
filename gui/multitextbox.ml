open! Widget

module TextState = struct
    type t = {
        rope : Rope.t;
        cursor_abs : int;
    }

    let empty = {
        rope = Rope.empty;
        cursor_abs = 0;
    }

	let rope_insert start rope r =
	  let open Rope in
	  concat2 (concat2 (sub r 0 start) rope) (sub r start (length r - start))

	let rope_remove start len r =
	  let open Rope in
	  concat2 (sub r 0 start) (sub r (start + len) (length r - start - len))

    let insert t c =
		let rope = rope_insert t.cursor_abs c t.rope in
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
		if Rope.length t.rope > 0 && t.cursor_abs > 0 then {
			rope = rope_remove (t.cursor_abs-1) 1 t.rope;
			cursor_abs = t.cursor_abs - 1;
		} else t

	let move_cursor_right t =
		if t.cursor_abs < Rope.length t.rope then {
			t with cursor_abs = t.cursor_abs + 1;
		} else t
	;;

	let move_cursor_up t =
		(* Look backwards for a '\n' *)
		let newline = '\n' in
		let cursor_abs = t.cursor_abs in
		let rindex_from pos =
			try Rope.rindex_from t.rope pos newline 
            with Caml.Not_found | Rope.Out_of_bounds _ -> -1
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
		let newline = '\n' in
		let cursor_abs = t.cursor_abs in
		let index_from pos = 
			try 
				Rope.index_from t.rope pos newline 
			with Caml.Not_found -> Rope.length t.rope 
		in
		let rindex_from pos = try Rope.rindex_from t.rope pos newline with Caml.Not_found -> -1 in
		let index = cursor_abs - rindex_from (cursor_abs - 1) in
		let next_line = index_from cursor_abs in
		let next_next_line = index_from (next_line + 1) in
		Printf.printf "NL %d NLL %d C %d\n" next_line next_next_line cursor_abs;
		if next_line = Rope.length t.rope then t
		else { t with cursor_abs = min next_next_line (next_line + index) }
	;;

	let iter t f =
		Rope.iter f t.rope

	let rope_fold t ~init ~f =
		let iter = Rope.Iterator.make t 0 in
		let rec loop acc =
			try 
				let acc = f acc (Rope.Iterator.get iter) in
				Rope.Iterator.incr iter;	
				loop acc;
			with Not_found | Rope.Out_of_bounds _ -> acc
		in
		loop init
	;;

	let longest_line t =
		let max, _, num_lines =
			rope_fold ~f:(fun (max_len, cur_len, lines) c ->
				match c with
				| '\n' -> (max max_len cur_len, 0, lines+1)
				| _ -> 
					let cur_len = cur_len + 1 in
					(max max_len cur_len, cur_len, lines)
			) ~init:(0, 0, 1) t.rope
		in
		max, num_lines
	;;

	let cursor_pos t = t.cursor_abs

	let to_string t = Rope.to_string t.rope
	let of_string str = {
		rope = Rope.of_string str;
		cursor_abs = String.length str;
	}
end

module TextStateMutable = struct
	type t = {
		mutable state : TextState.t;
	}

	let create () = {
		state = TextState.empty;
	}

	let insert t c =
		t.state <- TextState.insert t.state (Rope.of_string c)

	let longest_line t =
		TextState.longest_line t.state

	let cursor_pos t =
		TextState.cursor_pos t.state

	let to_string t =
		TextState.to_string t.state

	let of_string s = {
		state = TextState.of_string s
	}

	let move_cursor_left t =
		t.state <- TextState.move_cursor_left t.state

	let move_cursor_right t =
		t.state <- TextState.move_cursor_right t.state

	let move_cursor_up t =
		t.state <- TextState.move_cursor_up t.state

	let move_cursor_down t =
		t.state <- TextState.move_cursor_down t.state

	let delete_at_cursor t =
		t.state <- TextState.delete_at_cursor t.state
	
	let iter t =
		TextState.iter t.state
end

module TSM = TextStateMutable

class multilineTextbox_inner parent = object(self)
    inherit widget parent as _super

	val mutable editable : bool = true
	val mutable selectionPos : int = ~-1
	val textState : TSM.t = TSM.create()

	method! preferredSize _ctx =
        let longest, lines = TSM.longest_line textState in
        let fs = self#fontSize in
        let width = Float.max (10. *. fs) (float longest *. fs) in
        let height = Float.max (10. *. fs) (float lines *. fs) in
        Vec2.mk width height

	method! keyboardEvent ~key ~scancode:_ ~action _mods =
		if editable && focused then (
			if action = GLFW.Press || action = GLFW.Repeat then (
				begin match key with
				| GLFW.Enter -> 
                        let a, b = TSM.(longest_line textState) in
                        Printf.printf "Longest line? %d %d\n%!" a b;
                        TSM.insert textState "\n";
                        self#markLayoutDirty;
				| Backspace -> TSM.delete_at_cursor textState;
				| Up -> TSM.move_cursor_up textState
				| Left -> TSM.move_cursor_left textState
				| Right -> TSM.move_cursor_right textState
				| Down -> TSM.move_cursor_down textState
				| _ -> ()
				end;
				true
			) else false
		) else false

    method! keyboardCharacterEvent codepoint =
        if editable && focused then (
            (*self#deleteSelection |> ignore;*)
            begin match Char.of_int codepoint with
            | None -> 
				Caml.print_endline "NO CHAR";
				()
            | Some ch -> TSM.insert textState (Char.to_string ch);
            end;
            true
        ) else
            false

	method! draw ctx =
		let open Nanovg in
		
		begin_path ctx;	
		rect ctx 0. 0. size.a size.b;
		fill_color ctx (rgba 32 32 32 255);
		fill ctx;

        font_size ctx self#fontSize;
		fill_color ctx (rgb 255 255 255);
		text_align ctx Align.(left lor top);
		text_box ctx 0. 0. size.a TSM.(to_string textState) null_char |> ignore;
end

open Scrollpanel

class multilineTextbox parent = object(self)
    inherit widget parent as _super

    val mutable scroller = new scrollpanel None
    val mutable textBox = new multilineTextbox_inner None

    initializer
        self#addChildWidget (scroller :> widget);
        scroller#addChildWidget (textBox :> widget);

    method! preferredSize ctx =
        scroller#preferredSize ctx
end
