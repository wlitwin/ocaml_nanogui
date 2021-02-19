open Widget

type button_type = Normal
                 | Radio
                 | Toggle
                 | Popup

type icon_position = Left
                   | Right

type icon = FontIcon of string
          | Image of int
          | NoImage

class button parent caption icon = object(self)
    inherit widget parent as super

    val mutable caption : string = caption
    val mutable icon : icon = icon
    val mutable bgColor : Nanovg.color Ctypes.structure = Nanovg.(rgb 32 32 32)
    val mutable textColor : Nanovg.color Ctypes.structure = Nanovg.(rgb 255 255 255)
    val mutable iconPosition : icon_position = Left
    val mutable pushed : bool = false
    val mutable callback : unit -> unit = (fun () -> ())

    method caption = caption
    method setCaption c = caption <- c

    method backgroundColor = bgColor
    method setBackgroundColor b = bgColor <- b

    method textColor = textColor
    method setTextColor c = textColor <- c

    method icon = icon
    method setIcon i = icon <- i

    method iconPosition = iconPosition
    method setIconPosition p = iconPosition <- p

    method pushed = pushed
    method setPushed p = pushed <- p

    method callback = callback
    method setCallback c = callback <- c

    method! fontSize =
        match fontSize with
        | None -> theme#buttonFontSize
        | Some fs -> fs

    method private calcIconSize ctx fontsize =
        match icon with
        | NoImage -> 0.
        | FontIcon icon ->
            Nanovg.font_size ctx fontsize;
            Nanovg.font_face ctx "icons";
            Nanovg.text_bounds ctx 0. 0. icon Nanovg.null_char Nanovg.null_float
        | Image img ->
            let size = Ctypes.(allocate_n int ~count:2) in
            Nanovg.image_size ctx img size Ctypes.(size +@ 1);
            let w = float Ctypes.(!@size) in 
            let h = float Ctypes.(!@(size +@ 1)) in
            w*.fontsize/.h

    method! preferredSize ctx =
        let open Nanovg in
        let fontsize =
            if self#hasFontSize then self#fontSize
            else theme#buttonFontSize
        in
        Nanovg.font_size ctx fontsize;
        Nanovg.font_face ctx "mono";
        let tw = text_bounds ctx 0. 0. caption null_char null_float in
        let iw = self#calcIconSize ctx fontsize in
        Vec2.{a = tw+.iw+.20.; b = fontsize +. 10.}

    method! mouseButtonEvent ~pos button down _mods =
        if (button = GLFW.mouse_button_left) && enabled then (
            if down then (
                pushed <- true
            ) else if pushed then (
                if self#contains pos then (
                    callback ();
                ) else (
                    Printf.printf "Does not contain %.2f %.2f (%.2f %.2f - %.2f %.2f)\n%!" pos.a pos.b
                    position.a position.b size.a size.b
                    ;
                );
                pushed <- false;
            );
            
            true
        ) else (
            false
        )

    method! draw ctx =
        super#draw ctx;
        
        let grad_top, grad_bot =
            if pushed then (
                theme#buttonGradientTopPushed,
                theme#buttonGradientBotPushed
            ) else if mouseFocus && enabled then (
                theme#buttonGradientTopFocused,
                theme#buttonGradientBotFocused 
            ) else (
                theme#buttonGradientTopUnfocused,
                theme#buttonGradientBotUnfocused
            )
        in
        let corner_radius = theme#buttonCornerRadius in

        let open Nanovg in
        begin_path ctx;
        rounded_rect ctx 1. 1. (size.a-.2.) (size.b-.2.) (corner_radius-.1.);

        let bg = linear_gradient ctx 0. 0. 0. (size.b) grad_top grad_bot in
        fill_paint ctx bg;
        fill ctx;

        begin_path ctx;
        stroke_width ctx 1.;
        rounded_rect ctx (+.0.5) ((if pushed then 0.5 else 1.5)) 
                         (size.a-.1.) (size.b -. 1. -. (if pushed then 0. else 1.)) corner_radius;

        stroke_color ctx theme#borderLight;
        stroke ctx;

        begin_path ctx;
        rounded_rect ctx (+.0.5) (+.0.5) (size.a-.1.) (size.b-.2.) corner_radius;
        stroke_color ctx theme#borderDark;
        stroke ctx;

        let fontsize = self#fontSize in
        let iw = self#calcIconSize ctx fontsize in
        font_size ctx fontsize;
        font_face ctx "mono";
        let tw = text_bounds ctx 0. 0. caption null_char null_float in
        let center = Vec2.((size * 0.5)) in
        let text_color = if enabled then textColor else theme#disabledTextColor in

        let text_pos, icon_pos = (*Vec2.mk (center.a -. tw*.0.5) (center.b-.1.) in*)
            match icon with
            | NoImage -> Vec2.mk (center.a -. tw*.0.5) (center.b-.1.), Vec2.zero
            | _ ->
                match iconPosition with
                | Left ->
                    Vec2.mk (center.a -. tw*.0.5 +. iw) (center.b -. 1.), 
                    Vec2.mk (iw +. 3.) (center.b -. 1.)
                | Right ->
                    Vec2.mk (center.a -. tw*.0.5 -. iw) (center.b -. 1.),
                    Vec2.mk (size.a -. iw -. 3.) (center.b -. 1.)
        in

        begin match icon with
        | NoImage -> ()
        | FontIcon icon ->
            font_face ctx "icons";
            font_size ctx (fontsize*.2.0);
            fill_color ctx (rgba 255 255 255 255);
            text_align ctx Align.(center lor middle);
            text ctx icon_pos.a icon_pos.b icon null_char |> ignore;
        | Image img ->
                let paint = image_pattern ctx 
                    icon_pos.a (icon_pos.b -. fontsize*.0.5) 
                    iw fontsize 0. img (if enabled then 0.5 else 0.2) 
                in
                fill_paint ctx paint;
                fill ctx;
        end;

        font_size ctx fontsize;
        font_face ctx "mono";
        text_align ctx Align.(left lor middle);
        fill_color ctx theme#textColorShadow;
        text ctx text_pos.a text_pos.b caption null_char |> ignore;
        fill_color ctx text_color;
        text ctx text_pos.a (text_pos.b+.1.) caption null_char |> ignore;
end
