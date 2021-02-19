open Widget

module NV = Nanovg

class label parent text font_size = object(self)
    inherit widget parent as super

    val mutable text : string = text
    val mutable color : color = NV.rgb 200 200 200

    method color = color
    method setColor c = color <- c

    method text = text
    method setText t = 
        if String.length t <> String.length text then (
            self#markLayoutDirty;
        );
        text <- t;

    initializer
        self#setFontSize font_size

    method! preferredSize ctx = 
        if String.is_empty text then Vec2.zero
        else (
            NV.font_face ctx "mono";
            NV.font_size ctx self#fontSize;
            match self#fixedSize with
            | Some fs ->
                let bounds = Ctypes.(allocate_n float ~count:4) in
                NV.text_align ctx NV.Align.(left lor top);
                NV.text_box_bounds ctx 0. 0. fs.a text NV.null_char bounds;
                let _, miny, _, maxy = split_bounds bounds in
                Vec2.(mk fs.a (maxy -. miny))
            | None ->
                NV.text_align ctx NV.Align.(left lor middle);
                let bounds = NV.text_bounds ctx 0. 0. text NV.null_char NV.null_float +. 2. in
                Vec2.(mk bounds self#fontSize);
        )

    method! draw ctx =
        super#draw ctx;
        NV.font_face ctx "mono";
        NV.font_size ctx self#fontSize;
        NV.fill_color ctx color;
        match self#fixedSize with
        | Some fs ->
            NV.text_align ctx NV.Align.(left lor top);
            NV.text_box ctx 0. (fs.b*.0.5) fs.a text NV.null_char;
        | None ->
            NV.text_align ctx NV.Align.(left lor middle);
            NV.text ctx 0. (size.b*.0.5) text NV.null_char |> ignore;
end
