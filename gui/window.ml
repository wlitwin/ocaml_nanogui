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

        let open Gv in
        save ctx;
        Path.begin_ ctx;
        Path.rounded_rect ctx ~x:position.a ~y:position.b ~w:size.a ~h:size.b ~r:cr;
        set_fill_color ctx ~color:(if mouseFocus then theme#windowFillFocused else theme#windowFillUnfocused);
        fill ctx;

        (* Drop shadow *)
        let shadow_paint = Paint.box_gradient ctx ~x:position.a ~y:position.b 
            ~w:size.a ~h:size.b ~r:(cr*.2.) ~f:(ds*.2.) 
            ~icol:theme#dropShadow 
            ~ocol:theme#transparent 
        in
        save ctx;
        Scissor.reset ctx;
        Path.begin_ ctx;
        Path.rect ctx ~x:(position.a-.ds) ~y:(position.b-.ds) ~w:(size.a+.2.*.ds) ~h:(size.b*.2.*.ds);
        Path.rounded_rect ctx ~x:position.a ~y:position.b ~w:size.a ~h:size.b ~r:cr;
        Path.winding ctx ~winding:Winding.CW;
        set_fill_paint ctx ~paint:shadow_paint;
        fill ctx;
        restore ctx;

        if not String.(is_empty title) then (
            (* Header *)
            let header_paint = Paint.linear_gradient ctx ~sx:position.a ~sy:position.b ~ex:position.a 
                ~ey:(position.b+.size.b +. hh) ~icol:theme#windowHeaderGradientTop ~ocol:theme#windowHeaderGradientBot
            in

            Path.begin_ ctx;
            Path.rounded_rect ctx ~x:position.a ~y:position.b ~w:size.a ~h:hh ~r:cr;
            set_fill_paint ctx ~paint:header_paint;
            fill ctx;

            Path.begin_ ctx;
            Path.rounded_rect ctx ~x:position.a ~y:position.b ~w:size.a ~h:hh ~r:cr;
            set_stroke_color ctx ~color:theme#windowHeaderSepTop;
            stroke ctx;

            Text.set_size ctx ~size:18.;
            Text.set_font_face ctx ~name:"mono";
            Text.set_align ctx ~align:Align.(center lor middle);
            
            Text.set_blur ctx ~blur:2.;
            set_fill_color ctx ~color:theme#dropShadow;
            Text.text ctx ~x:(position.a+.size.a*.0.5) ~y:(position.b+.hh*.0.5) title;
            
            Text.set_blur ctx ~blur:0.;
            set_fill_color ctx ~color:(if focused then theme#windowTitleFocused else theme#windowTitleUnfocused);
            Text.text ctx ~x:(position.a+.size.a*.0.5) ~y:(position.b+.hh*.0.5-.1.) title;
        );

        restore ctx;
        super#draw ctx;


    method! mouseDragEvent ~pos:_ ~rel button _mods =
        if drag && (button = Mouse.button_left) then (
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
        ) else if (button = Mouse.button_left) then (
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
        let open Gv in
        Text.set_size ctx ~size:18.;
        Text.set_font_face ctx ~name:"mono";
        let b = Text.bounds ctx ~x:0. ~y:0. title in

        let minx = b.box.xmin in
        let miny = b.box.ymin in
        let maxx = b.box.xmax in
        let maxy = b.box.xmin in

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
