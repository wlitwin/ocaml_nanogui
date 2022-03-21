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
        if enabled && (button = Mouse.button_left) then (
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
            let open Gv in
            let fsize = self#fontSize in
            Text.set_size ctx ~size:fsize;
            Text.set_font_face ctx ~name:"mono";
            let bounds = (Text.bounds ctx ~x:0. ~y:0. caption).advance in
            Vec2.mk (bounds +. 1.8*.fsize) (fsize*.1.3)

    method! draw ctx =
        super#draw ctx;
        let open Gv in
        let fsize = self#fontSize in
        Text.set_size ctx ~size:fsize;
        Text.set_font_face ctx ~name:"mono";
        set_fill_color ctx ~color:(if enabled then theme#textColor else theme#disabledTextColor);
        Text.set_align ctx ~align:Align.(left lor middle);
        Text.text ctx ~x:(1.6*.fsize) ~y:(size.b*.0.5) caption;

        let bg = Paint.box_gradient ctx ~x:1.5 ~y:1.5 ~w:(size.b-.2.) ~h:(size.b-.2.) ~r:3. ~f:3.
            ~icol:(if pushed then rgba 0 0 0 100 
            else if mouseFocus then rgba 0 0 0 64
            else rgba 0 0 0 32)
            ~ocol:(rgba 0 0 0 180)
        in

        Path.begin_ ctx;
        Path.rounded_rect ctx ~x:1. ~y:1. ~w:(size.b-.2.) ~h:(size.b-.2.) ~r:3.;
        set_fill_paint ctx ~paint:bg;
        fill ctx;

        if checked then (
            Text.set_size ctx ~size:(size.b*.2.);
            Text.set_font_face ctx ~name:"icons";
            set_fill_color ctx ~color:(if enabled then theme#iconColor else theme#disabledTextColor);
            Text.set_align ctx ~align:Align.(center lor middle);
            Text.text ctx ~x:(size.b*.0.5) ~y:(size.b*.0.5) Entypo.check;
        )
end
