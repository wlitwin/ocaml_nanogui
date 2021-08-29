open! Widget

class graph parent = object
   inherit widget parent as super

   val mutable data : float array = [||]
   val mutable dataLen : float = 0.
   val mutable bgColor : color = Gv.Color.rgba ~r:20 ~g:20 ~b:20 ~a:128
   val mutable fgColor : color = Gv.Color.rgba ~r:255 ~g:192 ~b:0 ~a:128
   val mutable textColor : color = Gv.Color.rgba ~r:255 ~g:255 ~b:255 ~a:255
   val mutable header : string = ""
   val mutable footer : string = ""
   val mutable caption : string = ""
   val mutable fillUnderArea : bool = true

   method textColor = textColor
   method setTextColor t = textColor <- t

   method footer = caption
   method setFooter c = footer <- c

   method caption = caption
   method setCaption c = caption <- c

   method header = header
   method setHeader h = header <- h

   method fillUnderArea = fillUnderArea
   method setFillUnderArea a = fillUnderArea <- a

   method data = data
   method setData d = 
       data <- d;
       dataLen <- Array.length d |> Float.of_int;

   method foreColor = fgColor
   method setForeColor c = fgColor <- c

   method backColor = bgColor
   method setBackColor b = bgColor <- b

   method! preferredSize _ctx =
       Vec2.mk 180. 45.

    method! draw ctx =
        super#draw ctx;
        
        if Array.length data >= 2 then (
            let open Gv in
            Path.begin_ ctx;
            Path.rect ctx ~x:0. ~y:0. ~w:size.a ~h:size.b;
            set_fill_color ctx ~color:bgColor;
            fill ctx;

            Path.begin_ ctx;
            Path.move_to ctx ~x:0. ~y:(0. +. size.b);
            Array.iteri (fun idx value ->
                let open Float in
                let i_f = of_int idx in
                let vx = i_f*size.a / (dataLen-1.) in
                let vy = (1. - value) * size.b in
                Path.line_to ctx ~x:vx ~y:vy
            ) data;

            Path.line_to ctx ~x:size.a ~y:size.b;
            set_stroke_color ctx ~color:(Color.rgba ~r:100 ~g:100 ~b:100 ~a:255);
            stroke ctx;
            if fillUnderArea then (
                set_fill_color ctx ~color:fgColor;
                fill ctx;
            ) else (
                Path.close ctx;
            );

            Text.set_font_face ctx ~name:"mono";

            let render_text value size x y align =
                if not String.(is_empty value) then (
                    Text.set_size ctx ~size;
                    Text.set_align ctx ~align;
                    set_fill_color ctx ~color:textColor;
                    Text.text ctx ~x ~y value;
                );
            in

            render_text caption 14. 3. 1. Align.(left lor top);
            render_text header 18. (size.a -. 3.) 1. Align.(right lor top);
            render_text footer 15. (size.a -. 3.) (size.b +. 1.) Align.(right lor bottom);

            Path.begin_ ctx;
            Path.rect ctx ~x:0. ~y:0. ~w:size.a ~h:size.b;
            set_stroke_color ctx ~color:(Color.rgba ~r:100 ~g:100 ~b:100 ~a:255);
            stroke ctx;
        )
end
