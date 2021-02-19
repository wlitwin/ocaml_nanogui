open Widget

class window parent title = object(self)
    inherit widget parent as super

    val mutable title : string = title
    val mutable modal : bool = false
    val mutable panel : widget option = None
    val mutable drag : bool = false

    method title = title
    method setTitle t = title <- t

    method modal = modal
    method setModal m = modal <- m

    method buttonPanel = 
        match panel with
        | Some p -> p
        | None ->
            let bp = new widget (Some (self :> widget)) in
            panel <- Some bp;
            (* 
            panel#setLayout (new boxLayout horizontal middle 0 4)
            *)
            bp

    method center =
        ()
        (*
        match super#screen with
        | Some screen -> screen#centerWindow (self :> window)
        | None -> ()
        *)

    method! draw ctx =
        let ds = theme#windowDropShadowSize in 
        let cr = theme#windowCornerRadius in
        let hh = theme#windowHeaderHeight in

        let open Nanovg in
        save ctx;
        begin_path ctx;
        rounded_rect ctx position.a position.b size.a size.b cr;
        fill_color ctx (if mouseFocus then theme#windowFillFocused else theme#windowFillUnfocused);
        fill ctx;

        (* Drop shadow *)
        let shadow_paint = box_gradient ctx position.a position.b 
            size.a size.b (cr*.2.) (ds*.2.) theme#dropShadow theme#transparent 
        in
        save ctx;
        reset_scissor ctx;
        begin_path ctx;
        rect ctx (position.a-.ds) (position.b-.ds) (size.a+.2.*.ds) (size.b*.2.*.ds);
        rounded_rect ctx position.a position.b size.a size.b cr;
        path_winding ctx Solidity.(hole);
        fill_paint ctx shadow_paint;
        fill ctx;
        restore ctx;

        if not String.(is_empty title) then (
            (* Header *)
            let header_paint = linear_gradient ctx position.a position.b position.a 
                (position.b+.size.b +. hh) theme#windowHeaderGradientTop theme#windowHeaderGradientBot
            in

            begin_path ctx;
            rounded_rect ctx position.a position.b size.a hh cr;
            fill_paint ctx header_paint;
            fill ctx;

            begin_path ctx;
            rounded_rect ctx position.a position.b size.a hh cr;
            stroke_color ctx theme#windowHeaderSepTop;
            stroke ctx;

            font_size ctx 18.;
            font_face ctx "mono";
            text_align ctx Align.(center lor middle);
            
            font_blur ctx 2.;
            fill_color ctx theme#dropShadow;
            text ctx (position.a+.size.a*.0.5) (position.b+.hh*.0.5) title null_char |> ignore;
            
            font_blur ctx 0.;
            fill_color ctx (if focused then theme#windowTitleFocused else theme#windowTitleUnfocused);
            text ctx (position.a+.size.a*.0.5) (position.b+.hh*.0.5-.1.) title null_char |> ignore;
        );

        restore ctx;
        super#draw ctx;


    method! mouseDragEvent ~pos:_ ~rel button _mods =
        if drag && (button = GLFW.mouse_button_left) then (
            let parent_size = match parent with
                            | None -> Vec2.zero
                            | Some p -> p#size
            in
            position <- Vec2.(position + rel);
            position <- Vec2.(max position zero);
            position <- Vec2.(min (parent_size - size) position);
            true
        ) else 
            false

    method! mouseButtonEvent ~pos button down mods =
        if super#mouseButtonEvent ~pos button down mods then (
            true
        ) else if (button = GLFW.mouse_button_left) then (
            drag <- down && ((pos.b -. position.b) < theme#windowHeaderHeight);
            true
        ) else false

    method! scrollEvent ~pos ~rel =
        super#scrollEvent ~pos ~rel |> ignore;
        true

    method! preferredSize ctx =
        begin match panel with None -> () | Some p -> p#setVisible false end;
        let sz = super#preferredSize ctx in
        begin match panel with None -> () | Some p -> p#setVisible true end;
        let open Nanovg in
        font_size ctx 18.;
        font_face ctx "mono";
        let bounds = Ctypes.(allocate_n float ~count:4) in
        text_bounds ctx 0. 0. title null_char bounds |> ignore;

        let minx = Ctypes.(!@bounds) in
        let miny = Ctypes.(!@(bounds +@ 1)) in
        let maxx = Ctypes.(!@(bounds +@ 2)) in
        let maxy = Ctypes.(!@(bounds +@ 3)) in

        Printf.printf "%f %f %f %f\n%!" minx maxx miny maxy;

        let bounds = Vec2.{a = maxx -. minx +. 20.; b = maxy -. miny} in
        Vec2.{a = Float.max bounds.a sz.a; b = Float.max bounds.b sz.b}

    method! performLayout ctx =
        match panel with
        | None -> 
            V.iter (fun child ->
                child#setPosition Vec2.(mk 40. 32.);
            ) children;
            super#performLayout ctx
        | Some panel ->
            panel#setVisible false;
            super#performLayout ctx;
            V.iter (fun child ->
                child#setFixedSize Vec2.{a=22.; b=22.}
            ) panel#children;
            panel#setVisible true;
            panel#setSize Vec2.{a=self#width; b=22.};
            let panelSize = panel#preferredSize ctx in
            panel#setPosition Vec2.{a=self#width -. (panelSize.a +. 5.); b=3.};
            panel#performLayout ctx

    method private refreshRelativePlacement =
        ()
end
