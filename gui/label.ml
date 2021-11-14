open Widget

class label parent text font_size = object(self)
    inherit widget parent as super

    val mutable text : string = text
    val mutable multiline : bool = false
    val mutable color : color = Gv.Color.rgb ~r:200 ~g:200 ~b:200

    method color = color
    method setColor c = color <- c
    method mutliline = multiline

    method text = text
    method setMultiline m = multiline <- m
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
            Gv.Text.set_font_face ctx ~name:"mono";
            Gv.Text.set_size ctx ~size:self#fontSize;
            match self#fixedSize with
            | Some fs ->
                Gv.Text.set_align ctx ~align:Gv.Align.(left lor top);
                let b = Gv.Text.box_bounds ctx ~x:0. ~y:0. ~break_width:fs.a text in
                Vec2.(mk fs.a (b.ymax -. b.ymin))
            | None ->
                Gv.Text.set_align ctx ~align:Gv.Align.(left lor middle);
                let bounds = (Gv.Text.bounds ctx ~x:0. ~y:0. text).advance +. 2. in
                Vec2.(mk bounds self#fontSize);
        )

    method! draw ctx =
        super#draw ctx;
        Gv.Text.set_font_face ctx ~name:"mono";
        Gv.Text.set_size ctx ~size:self#fontSize;
        Gv.set_fill_color ctx ~color;
        if multiline then (
            Gv.Text.set_align ctx ~align:Gv.Align.(left lor top);
            Gv.Text.text_box ctx ~x:0. ~y:0. ~break_width:self#size.a text;
        ) else (
            match self#fixedSize with
            | Some fs ->
                Gv.Text.set_align ctx ~align:Gv.Align.(left lor top);
                Gv.Text.text_box ctx ~x:0. ~y:0. ~break_width:fs.a text;
            | None ->
                Gv.Text.set_align ctx ~align:Gv.Align.(left lor middle);
                Gv.Text.text ctx ~x:0. ~y:(size.b*.0.5) text;
        )
end
