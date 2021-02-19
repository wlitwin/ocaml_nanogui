open! Widget
open Float

class glcanvas parent = object(self)
    inherit widget parent as super

    val mutable bgColor : color = Nanovg.rgb 0 0 0
    val mutable drawBorder : bool = true
    val mutable drawCallback : graphics_context -> unit = (fun _ -> ())

    method drawCallback = drawCallback
    method setDrawCallback s = drawCallback <- s

    method backgroundColor = bgColor
    method setBackgroundColor b = bgColor <- b

    method drawBorder = drawBorder
    method setDrawBorder s = drawBorder <- s

    method drawBorderHelper (ctx : graphics_context) : unit =
        let open Nanovg in
        let corner = theme#windowCornerRadius in
        begin_path ctx;
        stroke_width ctx 1.;
        stroke_color ctx theme#borderLight;
        rounded_rect ctx 0.5 0.5 (size.a - 1.) (size.b - 1.) corner;
        stroke_color ctx theme#borderDark;
        rounded_rect ctx 0. 0. (size.a - 1.5) (size.b - 1.5) corner;
        stroke ctx;

    method drawGL (ctx : graphics_context) =
        drawCallback ctx

    method! draw ctx =
        super#draw ctx;

        if drawBorder then (
            self#drawBorderHelper ctx
        );
        (*end_frame ctx; (* Apparently you dont need to recall begin frame... *)*)

        let screen = Option.value_exn self#screen in

        let ratio = screen#pixelRatio in
        let screenSize = screen#size in
        let posOnScreen = self#absolutePosition in

        let size = Vec2.(size * ratio) in
        let imagePosition = Vec2.mk posOnScreen.a ((screenSize.b -. posOnScreen.b - size.b) * ratio) in

        let open Tgles2 in
        let stored_viewport = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 4 in
        Gl.(get_integerv viewport_enum stored_viewport);

        let x, y, w, h = 
            imagePosition.a |> to_int,
            imagePosition.b |> to_int,
            size.a |> to_int,
            size.b |> to_int
        in

        let px, py, pw, ph =
            match parent with
            | None -> x, y, w, h
            | Some p ->
                    (*
                let pos = self#absolutePosition in
                pos.a |> to_int,
                (screenSize.b - pos.b - size.b) |> to_int,
                p#size.a |> to_int,
                p#size.b |> to_int
                *)
                let pos = p#absolutePosition in
                let sz = p#size in
                let py = (screenSize.b - pos.b - sz.b)*ratio in
                pos.a |> to_int, 
                py |> to_int, 
                sz.a |> to_int, 
                sz.b |> to_int
        in

        (*
        begin_path ctx;
        fill_color ctx (rgba 255 0 0 255);
        rect ctx 0. 0. size.a size.b;
        fill ctx;
        *)

        Gl.viewport x y w h;
        Gl.enable Gl.scissor_test;
        if (ph < h && pw < w) then (
            let open Int in
            (* TODO - need to communicate these extra bits *)
            Gl.scissor px (py+12) (pw-12) (ph-12);
        ) else if (ph < h) then (
            Gl.scissor px py (Int.min w pw) (Int.min h ph);
        ) else if (pw < w) then (
            Gl.scissor px y Int.(min w pw) h;
        ) else ( 
            Gl.scissor x y w h;
         ); 
        let module CH = ColorHelper in
        Gl.clear_color CH.(r bgColor) CH.(g bgColor) CH.(b bgColor) CH.(a bgColor);
        Gl.clear Gl.(color_buffer_bit lor depth_buffer_bit lor stencil_buffer_bit);
        
        self#drawGL ctx;

        Gl.disable Gl.scissor_test;
        let get idx = Bigarray.Array1.get stored_viewport idx |> Int32.to_int in
        Gl.viewport (get 0) (get 1) (get 2) (get 3);

end
