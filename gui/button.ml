open Widget

type button_type = Normal
                 | Radio
                 | Toggle
                 | Popup

type icon_position = Left
                   | Right

type icon = FontIcon of string
          | Image of Gv.Image.image
          | NoImage

class button parent caption icon = object(self)
    inherit widget parent as super

    val mutable caption : string = caption
    val mutable icon : icon = icon
    val mutable bgColor : color = Gv.Color.(rgb ~r:32 ~g:32 ~b:32)
    val mutable textColor : color = Gv.Color.(rgb ~r:255 ~g:255 ~b:255)
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
            Gv.Text.set_size ctx ~size:fontsize;
            Gv.Text.set_font_face ctx ~name:"icons";
            (Gv.Text.bounds ctx ~x:0. ~y:0. icon).advance
        | Image img ->
            let w, h = Gv.Image.size ctx img in
            let w = float w in
            let h = float h in
            w*.fontsize/.h

    method! preferredSize ctx =
        let open Gv in
        let fontsize =
            if self#hasFontSize then self#fontSize
            else theme#buttonFontSize
        in
        Gv.Text.set_size ctx ~size:fontsize;
        Gv.Text.set_font_face ctx ~name:"mono";
        let tw = (Text.bounds ctx ~x:0. ~y:0. caption).advance in
        let iw = self#calcIconSize ctx fontsize in
        Vec2.{a = tw+.iw+.20.; b = fontsize +. 10.}

    method! mouseButtonEvent ~pos button down _mods =
        if (button = Mouse.button_left) && enabled then (
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

        let open Gv in
        Path.begin_ ctx;
        Path.rounded_rect ctx ~x:1. ~y:1. ~w:(size.a-.2.) ~h:(size.b-.2.) ~r:(corner_radius-.1.);

        let bg = Paint.linear_gradient ctx ~sx:0. ~sy:0. ~ex:0. ~ey:(size.b) ~icol:grad_top ~ocol:grad_bot in
        set_fill_paint ctx ~paint:bg;
        fill ctx;

        Path.begin_ ctx;
        set_stroke_width ctx ~width:1.;
        Path.rounded_rect ctx ~x:(+.0.5) ~y:((if pushed then 0.5 else 1.5)) 
                         ~w:(size.a-.1.) ~h:(size.b -. 1. -. (if pushed then 0. else 1.)) ~r:corner_radius;

        set_stroke_color ctx ~color:theme#borderLight;
        stroke ctx;

        Path.begin_ ctx;
        Path.rounded_rect ctx ~x:(+.0.5) ~y:(+.0.5) ~w:(size.a-.1.) ~h:(size.b-.2.) ~r:corner_radius;
        set_stroke_color ctx ~color:theme#borderDark;
        stroke ctx;

        let fontsize = self#fontSize in
        let iw = self#calcIconSize ctx fontsize in
        Text.set_size ctx ~size:fontsize;
        Text.set_font_face ctx ~name:"mono";
        let tw = Text.bounds ctx ~x:0. ~y:0. caption in
        let tw = tw.advance in
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
            Text.set_font_face ctx ~name:"icons";
            Text.set_size ctx ~size:(fontsize*.2.0);
            set_fill_color ctx ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:255);
            Text.set_align ctx ~align:Align.(center lor middle);
            Text.text ctx ~x:icon_pos.a ~y:icon_pos.b icon;
        | Image img ->
                let paint = Paint.image_pattern ctx 
                    ~cx:icon_pos.a ~cy:(icon_pos.b -. fontsize*.0.5) 
                    ~w:iw ~h:fontsize ~angle:0. ~image:img ~alpha:(if enabled then 0.5 else 0.2) 
                in
                set_fill_paint ctx ~paint;
                fill ctx;
        end;

        Text.set_size ctx ~size:fontsize;
        Text.set_font_face ctx ~name:"mono";
        Text.set_align ctx ~align:Align.(left lor middle);
        set_fill_color ctx ~color:theme#textColorShadow;
        Text.text ctx ~x:text_pos.a ~y:text_pos.b caption;
        set_fill_color ctx ~color:text_color;
        Text.text ctx ~x:text_pos.a ~y:(text_pos.b+.1.) caption;
end
