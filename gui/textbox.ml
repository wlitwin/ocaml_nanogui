open Widget

type spin_area = None | Top | Bottom

let glyph_pos_at (glyphs : Gv.Text.glyph_position array) ~len ~index =
    if index < 0 || index >= len then 0.
    else glyphs.(index).x

let has_mod mods value = 
    List.mem mods value

let swap (a, b) = (b, a)

let string_delete_index str idx =
    let len = String.length str in
    if idx < 0 || idx >= len then str
    else if idx = 0 then String.suffix str (String.length str - 1)
    else if idx = len -1 then String.prefix str (String.length str - 1)
    else (
        String.slice str 0 idx ^ String.slice str (idx+1) 0
    )
;;

let%expect_test _ =
    Caml.print_endline (string_delete_index "" 0);
    Caml.print_endline (string_delete_index "" ~-1);
    Caml.print_endline (string_delete_index "" 5);
    Caml.print_endline (string_delete_index "hello" 0);
    Caml.print_endline (string_delete_index "hello" 5);
    Caml.print_endline (string_delete_index "hello" 4);
    Caml.print_endline (string_delete_index "hello" 2);
  [%expect {|
    ello
    hello
    hell
    helo |}]
;;

let insert_at str idx value =
    let len = String.length str in
    if idx < 0 || idx > len then str
    else (
        String.prefix str idx ^ value ^ String.suffix str (len - idx)
    )
;;

let%expect_test _ =
    Caml.print_endline (insert_at "" 0 "h");
    Caml.print_endline (insert_at "h" 1 "e");
    Caml.print_endline (insert_at "hello" 1 "z");
    Caml.print_endline (insert_at "hello" 5 "!");
  [%expect {|
    h
    he
    hzello
    hello! |}]
;;

let%expect_test _ =
    Caml.print_endline (insert_at "hello" 2 "world");
    Caml.print_endline (insert_at "hello" 0 "world");
    Caml.print_endline (insert_at "hello" 5 "world");
    Caml.print_endline (insert_at "hello" ~-1 "world");
    Caml.print_endline (insert_at "hello" 6 "world");
  [%expect {|
    heworldllo
    worldhello
    helloworld
    hello
    hello |}]
;;

class textbox parent in_value = object(self)
    inherit widget parent as super

    val mutable editable : bool = true
    val mutable alignment : alignment = Center
    val mutable placeholder : string = ""
    val mutable cursorPos : int = -1
    val mutable valueTemp : string = in_value
    val mutable selectionPos : int = -1
    val mutable mousePos : Vec2.t = Vec2.mk1 ~-.1.
    val mutable mouseDownPos : Vec2.t = Vec2.mk1 ~-.1.
    val mutable mouseDragPos : Vec2.t = Vec2.mk1 ~-.1.
    val mutable mouseDownModifier : Key.modifier list = []
    val mutable textOffset : float = 0.
    val mutable lastClick : float = 0.
    val mutable callback : string -> bool = (fun _ -> false)

    method setEditable e =
        editable <- e;
        (*
        let cursor = 
            Cursor.createStandardCursor ~shape:(
                if editable then Cursor.IBeamCursor
                else Cursor.ArrowCursor
            )
        in
        self#setCursor cursor
        *)

    method! setTheme t =
        super#setTheme t;
        fontSize <- Some t#textBoxFontSize;

    method callback = callback
    method setCallback c = callback <- c

    method! preferredSize ctx =
        let open Gv in
        let size = Vec2.mk 0. (self#fontSize*.1.4) in
        let ts = (Text.bounds ctx ~x:0. ~y:0. valueTemp).advance in
        Vec2.mk (size.b +. ts) size.b

    method pasteFromClipboard =  
        match self#screen with
        | None -> ()
        | Some screen ->
            let str = Clipboard.get_string ~window:screen#window in
            valueTemp <- insert_at valueTemp cursorPos str

    method! mouseButtonEvent ~pos button down mods = 
        if button = Mouse.button_left && down && not focused then (
            self#requestFocus
        );

        if editable && self#focused then (
            if down then (
                mouseDownPos <- pos;
                mouseDownModifier <- mods;

                let time = Time.now() in
                if (time -. lastClick < 0.25) then (
                    (* Double click - select all text *)
                    selectionPos <- 0;
                    cursorPos <- String.length valueTemp;
                    mouseDownPos <- Vec2.mk1 ~-.1.;
                );
                lastClick <- time;
            ) else (
                mouseDownPos <- Vec2.mk1 ~-.1.;
                mouseDragPos <- Vec2.mk1 ~-.1.;
            );
            true
        ) else
            false

    method! mouseMotionEvent ~pos ~rel:_ _button _mods =
        mousePos <- pos;
        (*
        if editable then (
            self#setCursor Cursor.(createStandardCursor ~shape:ArrowCursor);
        ) else if spinnable && not focused && Poly.(self#spinArea mousePos <> None) then (
            self#setCursor Cursor.(createStandardCursor ~shape:HandCursor);
        ) else (
            self#setCursor Cursor.(createStandardCursor ~shape:IBeamCursor);
        );
        *)

        if editable && focused 
        then true
        else false

    method! mouseDragEvent ~pos ~rel:_ _button _mods =
        mousePos <- pos;
        mouseDragPos <- pos;
        if editable && focused
        then true
        else false

    method! focusEvent ~focus =
        super#focusEvent ~focus |> ignore;

        let backup = valueTemp in
        if editable then (
            if focused then (
                cursorPos <- 0;
            ) else (
                if not (callback valueTemp) then (
                    valueTemp <- backup;
                );

                cursorPos <- ~-1;
                selectionPos <- ~-1;
                textOffset <- 0.;
            );
        );

        true

    method copySelection =
        if selectionPos > ~-1 then (
            match self#screen with
            | None -> false
            | Some screen ->
                let start, end_ =
                    if cursorPos > selectionPos then (
                        selectionPos, cursorPos
                    ) else
                        cursorPos, selectionPos
                in

                Clipboard.set_string
                    ~window:screen#window 
                    ~string:String.(sub valueTemp ~pos:start ~len:(end_ - start));
            true
        ) else false

    method deleteSelection =
        if selectionPos > ~-1 then (
            let start = cursorPos in
            let end_ = selectionPos in

            let start, end_ =
                if start > end_ then 
                    end_, start
                else start, end_
            in

            if start = end_ - 1 then (
                valueTemp <- string_delete_index valueTemp start
            ) else if start = 0 && end_ = String.length valueTemp then (
                valueTemp <- ""
            ) else (
                valueTemp <- 
                    String.slice valueTemp 0 start
                    ^ String.slice valueTemp end_ 0
            );


            cursorPos <- start;
            selectionPos <- ~-1;
            true
        ) else false

    method! keyboardEvent ~key ~scancode:_ ~action mods =
        let shift_down = has_mod mods Key.Shift in
        let ctrl_down = has_mod mods Key.Control in
        let reset_selection () =
            if shift_down then (
                if selectionPos = -1 then
                    selectionPos <- cursorPos
            ) else (
                selectionPos <- -1;
            )
        in
        if editable && focused then (
            if (action = Key.Press || action = Key.Repeat) then (
                begin match key with
                | Key.Left ->
                    reset_selection();
                    if cursorPos > 0 then
                        cursorPos <- cursorPos - 1
                | Right ->
                    reset_selection();
                    if cursorPos < String.length valueTemp then
                        cursorPos <- cursorPos + 1
                | Home ->
                    reset_selection();
                    cursorPos <- 0
                | End ->
                    reset_selection();
                    cursorPos <- String.length valueTemp
                | Backspace ->
                    if not self#deleteSelection && cursorPos > 0 then (
                        valueTemp <- string_delete_index valueTemp (cursorPos - 1);
                        cursorPos <- cursorPos - 1;
                    )
                | Delete -> 
                    if not self#deleteSelection && cursorPos < String.length valueTemp then (
                        valueTemp <- string_delete_index valueTemp cursorPos
                    )
                | Enter -> (*self#focusEvent ~focus:false |> ignore *) ()
                | A when shift_down ->
                    cursorPos <- String.length valueTemp;
                    selectionPos <- 0;
                | X when ctrl_down ->
                    self#copySelection |> ignore;
                    self#deleteSelection |> ignore;
                | C when ctrl_down -> 
                    self#copySelection |> ignore
                | V when ctrl_down ->
                    self#deleteSelection |> ignore;
                    self#pasteFromClipboard;
                | _ -> ()
                end;
            );
            true
        ) else false

    method! keyboardCharacterEvent codepoint =
        if editable && self#focused then (
            self#deleteSelection |> ignore;
            begin match Char.of_int codepoint with
            | None -> ()
            | Some ch ->
                valueTemp <- insert_at valueTemp cursorPos Char.(to_string ch);
                cursorPos <- cursorPos + 1;
            end;
            true
        ) else
            false

    method positionToCursorIndex posx lastx (glyphs : Gv.Text.glyph_position array) len =
        let cursorIdx = ref 0 in
        let caretx = glyph_pos_at glyphs ~len ~index:!cursorIdx |> ref in
        for j=1 to len-1 do
            let glyph_j = glyph_pos_at glyphs ~len ~index:j in
            if Float.(abs (!caretx - posx) > abs (glyph_j - posx)) then (
                cursorIdx := j;
                caretx := (glyph_pos_at glyphs ~len ~index:!cursorIdx);
            )
        done;

        if Float.(abs (!caretx - posx) > abs(lastx - posx)) then (
            cursorIdx := len;
        );
        !cursorIdx

    method cursorIndexToPosition (index : int) (lastx : float) glyphs (len : int) =
        if index < 0 then 0.
        else if index >= len then lastx
        else glyph_pos_at glyphs ~len ~index

    method updateCursor offset lastx (glyphs : Gv.Text.glyph_position array) size =
        if (mouseDownPos.a <> ~-.1.) then (
            if has_mod mouseDownModifier Key.Shift then (
                if selectionPos = ~-1 then (
                    selectionPos <- cursorPos
                );
            ) else (
                selectionPos <- ~-1;
            );

            cursorPos <- self#positionToCursorIndex (mouseDownPos.a -. offset) lastx glyphs size;
            mouseDownPos <- Vec2.mk1 ~-.1.;
        ) else if (mouseDragPos.a <> ~-.1.) then (
            if selectionPos = ~-1 then (
                selectionPos <- cursorPos
            );

            cursorPos <- self#positionToCursorIndex (mouseDragPos.a -. offset) lastx glyphs size
        ) else (
            if cursorPos = ~-2 then
                cursorPos <- size
        );

        if cursorPos = selectionPos then (
            selectionPos <- ~-1;
        )

    method drawBackground ctx =
        let open Gv in
        let open Float in

        let box_grad color1 color2 = Paint.box_gradient ctx 
            ~x:1. ~y:2. 
            ~w:(size.a-2.) ~h:(size.b-2.) ~r:3. ~f:4. 
            ~icol:color1 ~ocol:color2
        in

        let bg = box_grad (rgba 255 255 255 32) (rgba 32 32 32 32) in
        let fg2 = box_grad (rgba 150 150 150 32) (rgba 32 32 32 32) in

        Path.begin_ ctx;
        Path.rounded_rect ctx ~x:1. ~y:2. ~w:(size.a-2.) ~h:(size.b-2.) ~r:3.;

        if editable && self#focused then (
            set_fill_paint ctx ~paint:fg2
        ) else (
            set_fill_paint ctx ~paint:bg;
        );
        
        fill ctx;

        Path.begin_ ctx;
        Path.rounded_rect ctx ~x:0.5 ~y:0.5 ~w:(size.a-1.) ~h:(size.b-1.) ~r:2.5;
        set_stroke_color ctx ~color:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:48);
        stroke ctx;

    method value = valueTemp
    method setValue v =
        cursorPos <- ~-1;
        selectionPos <- ~-1;
        valueTemp <- v;
        self#markLayoutDirty;

    method! draw ctx =
        super#draw ctx;
        self#drawBackground ctx;
        let open Gv in
        let open Float in

        let align = match alignment with
                  | Left -> Align.(left lor middle)
                  | Right -> Align.(right lor middle)
                  | Center -> Align.(center lor middle)
        in

        let text_color = 
            if enabled && not (String.is_empty valueTemp)
            then (*theme#textColor*) (rgba 255 255 255 255)
            else (*theme#disabledTextColor*) (rgba 255 255 255 255)
        in

        let fontSize = self#fontSize in
        Text.set_align ctx ~align;
        Text.set_size ctx ~size:fontSize;

        let pad_x = 8. in
        let size = Vec2.mk (size.a - 2.*pad_x) size.b in

        let tw = (Text.bounds ctx ~x:0. ~y:0. valueTemp).advance in
        let center = Vec2.((size * 0.5)) in
        let text_pos = Vec2.mk (center.a -. tw*.0.5) (center.b-.1.) in

        (* compute text offset *)
        let max_glyphs : int = String.length valueTemp in
        let glyphs : Text.glyph_position array = 
            Stdlib.Array.make max_glyphs Text.empty_glyph_position 
        in
        let nglyphs = Text.glyph_positions ctx ~x:(tw*0.5) ~y:text_pos.b ~glyphs valueTemp in
        let b = Text.bounds ctx ~x:(tw*0.5) ~y:0. valueTemp in

        let maxx = b.box.xmax in
        let maxy = b.box.ymax in
        let miny = b.box.ymin in

        (* Drag, click, selections *)
        self#updateCursor (position.a - textOffset + pad_x) maxx glyphs nglyphs;

        (* Draw cursor *)
        if (cursorPos >= 0 && cursorPos <= nglyphs) then (
            let caretx = self#cursorIndexToPosition cursorPos maxx glyphs nglyphs in
            let prevCx = self#cursorIndexToPosition Int.(cursorPos - 1) maxx glyphs nglyphs in
            let line_h = (maxy - miny)*1.2  in
            let offset_y = (size.b - line_h)*0.5 in

            if (caretx - textOffset) > size.a then (
                textOffset <- (caretx - size.a);
            ) else if (prevCx - textOffset) < 0. then (
                textOffset <- textOffset + (prevCx - textOffset);
            );

            let caretx = caretx-textOffset+pad_x in

            (* Draw the selection *)
            if (selectionPos >= 0) then (
                let selx = (self#cursorIndexToPosition selectionPos maxx glyphs nglyphs) - textOffset + pad_x in
                let caretx, selx = if caretx > selx then selx, caretx else caretx, selx in

                Path.begin_ ctx;
                set_fill_color ctx ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:80);
                Path.rect ctx ~x:(caretx) ~y:offset_y ~w:(selx - caretx) ~h:line_h;
                fill ctx;
            );

            Path.begin_ ctx;
            Path.move_to ctx ~x:caretx ~y:offset_y;
            Path.line_to ctx ~x:caretx ~y:(size.b - offset_y);
            set_stroke_color ctx ~color:(Color.rgba ~r:255 ~g:192 ~b:0 ~a:255);
            set_stroke_width ctx ~width:1.5;
            stroke ctx;
        );

        set_fill_color ctx ~color:text_color;
        Text.text ctx ~x:(tw*0.5 - textOffset+pad_x) ~y:text_pos.b valueTemp;
end
