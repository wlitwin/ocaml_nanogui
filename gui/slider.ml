open! Widget

class slider parent ~min ~max ~value = object(self)
    inherit widget parent

    val mutable value = value
    val mutable minValue = min
    val mutable maxValue = max
    val mutable callback = (fun _ -> ())
    val mutable finalCallback = (fun _ -> ())

    method value = value
    method setValue v = value <- v

    method minValue = minValue
    method setMinValue v = minValue <- v

    method maxValue = maxValue
    method setMaxValue v = maxValue <- v

    method callback = callback
    method setCallback s = callback <- s

    method finalCallback = finalCallback
    method setFinalCallback s = finalCallback <- s

    method! preferredSize _ =
        Vec2.mk 70. 16.

    method private calcValue (pos : Vec2.t) =
        let kr = size.b*.0.4 in
        let kshadow = 3. in
        let start_x = kr +. kshadow +. position.a -. 1. in
        let width_x = size.a -. 2. *. (kr +. kshadow) in
        let tvalue = (pos.a -. start_x) /. width_x in
        let tvalue = tvalue *. (maxValue -. minValue) +. minValue in
        Float.(clamp_exn ~min:minValue ~max:maxValue tvalue)

    method! mouseDragEvent ~pos ~rel:_ _button _mods =
        if enabled then (
            value <- self#calcValue pos;
            callback value;
            true
        ) else false

    method! mouseButtonEvent ~pos _button down _mods =
        if enabled then (
            value <- self#calcValue pos;
            callback value;
            if not down then (
                finalCallback value;
            );
            true
        ) else false

    method! draw ctx =
        let open Nanovg in
        let open Float in
        let center = Vec2.(size * 0.5) in

        begin_path ctx;
        fill_color ctx (rgba 96 96 96 255);
        rect ctx 0. 0. size.a size.b;
        fill ctx;

        let rect_y = center.b - size.b*0.25 in
        let rect_h = size.b*0.5 in
        let kr = size.b*0.4 in
        let kshadow = 3. in
        let start_x = kr in
        let width_x = size.a - 2. * kr in

        let knob_pos = Vec2.mk
            (start_x + ((value - minValue) / (maxValue - minValue))*width_x)
            center.b
        in

        let bg = box_gradient ctx start_x rect_y width_x rect_h 3. 3. 
            (if enabled then (rgba 0 0 0 32) else (rgba 0 0 0 10))
            (if enabled then (rgba 0 0 0 128) else (rgba 0 0 0 210))
        in

        begin_path ctx;
        rounded_rect ctx start_x rect_y width_x rect_h 2.;
        fill_paint ctx bg;
        fill ctx;

        let knob_shadow = radial_gradient ctx knob_pos.a knob_pos.b 
            (kr-kshadow) (kr+kshadow) (rgba 0 0 0 64) theme#transparent
        in

        begin_path ctx;
        rect ctx (knob_pos.a - kr - 5.) (knob_pos.b - kr - 5.) 
                 (kr * 2. + 10.) (kr * 2. + 10. + kshadow);
        circle ctx knob_pos.a knob_pos.b kr;
        path_winding ctx Solidity.hole;
        fill_paint ctx knob_shadow;
        fill ctx;

        let knob = linear_gradient ctx 0. (center.b - kr) 0. (center.b + kr)
                    theme#borderLight theme#borderMedium
        in

        let knob_reverse = linear_gradient ctx 0. (center.b - kr) 0. (center.b + kr)
                    theme#borderMedium theme#borderLight
        in

        begin_path ctx;
        circle ctx knob_pos.a knob_pos.b kr;
        stroke_color ctx theme#borderDark;
        fill_paint ctx knob;
        stroke ctx;
        fill ctx;

        begin_path ctx;
        circle ctx knob_pos.a knob_pos.b (kr*0.5);
        fill_color ctx (rgba 150 150 150 (if enabled then 255 else 100));
        stroke_paint ctx knob_reverse;
        stroke ctx;
        fill ctx;
end
