open! Widget

class checkbox parent caption = object(self)
    inherit widget parent as super

    val mutable callback : bool -> unit = (fun _ -> ())
    val mutable pushed : bool = false
    val mutable checked : bool = false
    val mutable caption : string = caption

    method caption = caption
    method setCaption c = caption <- c

    method pushed = pushed
    method setPushed p = pushed <- p

    method isSet = self#checked
    method set = self#setChecked

    method checked = checked
    method setChecked c = checked <- c

    method changeCallback = callback
    method setChangeCallback c = callback <- c

    method! mouseButtonEvent ~pos button down _mods =
        if enabled && (button = GLFW.mouse_button_left) then (
            if down then (
                pushed <- true;
            ) else if pushed then (
                if self#contains pos then (
                    checked <- not checked;   
                    callback checked;
                );
                pushed <- false;
            );
            true
        ) else false

    method! preferredSize ctx =
        match self#fixedSize with
        | Some fs -> fs
        | None ->
            let open Nanovg in
            let fsize = self#fontSize in
            font_size ctx fsize;
            font_face ctx "mono";
            let bounds = text_bounds ctx 0. 0. caption null_char null_float in
            Vec2.mk (bounds +. 1.8*.fsize) (fsize*.1.3)

    method! draw ctx =
        super#draw ctx;
        let open Nanovg in
        let fsize = self#fontSize in
        font_size ctx fsize;
        font_face ctx "mono";
        fill_color ctx (if enabled then theme#textColor else theme#disabledTextColor);
        text_align ctx Align.(left lor middle);
        text ctx (1.6*.fsize) (size.b*.0.5) caption null_char |> ignore;

        let bg = box_gradient ctx 1.5 1.5 (size.b-.2.) (size.b-.2.) 3. 3.
            (if pushed then rgba 0 0 0 100 
            else if mouseFocus then rgba 0 0 0 64
            else rgba 0 0 0 32)
            (rgba 0 0 0 180)
        in

        begin_path ctx;
        rounded_rect ctx 1. 1. (size.b-.2.) (size.b-.2.) 3.;
        fill_paint ctx bg;
        fill ctx;

        if checked then (
            font_size ctx (size.b*.2.);
            font_face ctx "icons";
            fill_color ctx (if enabled then theme#iconColor else theme#disabledTextColor);
            text_align ctx Align.(center lor middle);
            text ctx (size.b*.0.5) (size.b*.0.5) (*to_utf8 theme#checkBoxIcon*) Entypo.check null_char |> ignore;
        )
end
