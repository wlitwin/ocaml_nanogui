open! Widget

module NV = Nanovg

class graph parent = object
   inherit widget parent as super

   val mutable data : float array = [||]
   val mutable dataLen : float = 0.
   val mutable bgColor : color = NV.rgba 20 20 20 128
   val mutable fgColor : color = NV.rgba 255 192 0 128
   val mutable textColor : color = NV.rgba 255 255 255 255
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
            let open NV in
            begin_path ctx;
            rect ctx 0. 0. size.a size.b;
            fill_color ctx bgColor;
            fill ctx;

            begin_path ctx;
            move_to ctx 0. (0. +. size.b);
            Array.iteri (fun idx value ->
                let open Float in
                let i_f = of_int idx in
                let vx = i_f*size.a / (dataLen-1.) in
                let vy = (1. - value) * size.b in
                line_to ctx vx vy
            ) data;

            line_to ctx size.a size.b;
            stroke_color ctx (rgba 100 100 100 255);
            stroke ctx;
            if fillUnderArea then (
                fill_color ctx fgColor;
                fill ctx;
            ) else (
                close_path ctx;
            );

            font_face ctx "mono";

            let render_text value size x y align =
                if not String.(is_empty value) then (
                    font_size ctx size;
                    text_align ctx align;
                    fill_color ctx textColor;
                    text ctx x y value null_char |> ignore;
                );
            in

            render_text caption 14. 3. 1. Align.(left lor top);
            render_text header 18. (size.a -. 3.) 1. Align.(right lor top);
            render_text footer 15. (size.a -. 3.) (size.b +. 1.) Align.(right lor bottom);

            begin_path ctx;
            rect ctx 0. 0. size.a size.b;
            stroke_color ctx (rgba 100 100 100 255);
            stroke ctx;
        )
end
