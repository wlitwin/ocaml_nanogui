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
        let open Gv in
        let open Float in
        let center = Vec2.(size * 0.5) in

        Path.begin_ ctx;
        set_fill_color ctx ~color:(Color.rgba ~r:96 ~g:96 ~b:96 ~a:255);
        Path.rect ctx ~x:0. ~y:0. ~w:size.a ~h:size.b;
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

        let bg = Paint.box_gradient ctx ~x:start_x ~y:rect_y ~w:width_x ~h:rect_h ~r:3. ~f:3. 
            ~icol:(if enabled then (Color.rgba ~r:0 ~g:0 ~b:0 ~a:32) else (Color.rgba ~r:0 ~g:0 ~b:0 ~a:10))
            ~ocol:(if enabled then (Color.rgba ~r:0 ~g:0 ~b:0 ~a:128) else (Color.rgba ~r:0 ~g:0 ~b:0 ~a:210))
        in

        Path.begin_ ctx;
        Path.rounded_rect ctx ~x:start_x ~y:rect_y ~w:width_x ~h:rect_h ~r:2.;
        set_fill_paint ctx ~paint:bg;
        fill ctx;

        let knob_shadow = Paint.radial_gradient ctx ~cx:knob_pos.a ~cy:knob_pos.b 
            ~in_radius:(kr-kshadow) ~out_radius:(kr+kshadow) 
            ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:64) ~ocol:theme#transparent
        in

        Path.begin_ ctx;
        Path.rect ctx ~x:(knob_pos.a - kr - 5.) ~y:(knob_pos.b - kr - 5.) 
                 ~w:(kr * 2. + 10.) ~h:(kr * 2. + 10. + kshadow);
        Path.circle ctx ~cx:knob_pos.a ~cy:knob_pos.b ~r:kr;
        Path.winding ctx ~winding:Winding.CW;
        set_fill_paint ctx ~paint:knob_shadow;
        fill ctx;

        let knob = Paint.linear_gradient ctx ~sx:0. ~sy:(center.b - kr) ~ex:0. ~ey:(center.b + kr)
                    ~icol:theme#borderLight ~ocol:theme#borderMedium
        in

        let knob_reverse = Paint.linear_gradient ctx ~sx:0. ~sy:(center.b - kr) ~ex:0. ~ey:(center.b + kr)
                    ~icol:theme#borderMedium ~ocol:theme#borderLight
        in

        Path.begin_ ctx;
        Path.circle ctx ~cx:knob_pos.a ~cy:knob_pos.b ~r:kr;
        set_stroke_color ctx ~color:theme#borderDark;
        set_fill_paint ctx ~paint:knob;
        stroke ctx;
        fill ctx;

        Path.begin_ ctx;
        Path.circle ctx ~cx:knob_pos.a ~cy:knob_pos.b ~r:(kr*0.5);
        set_fill_color ctx ~color:(Color.rgba ~r:150 ~g:150 ~b:150 ~a:(if enabled then 255 else 100));
        set_stroke_paint ctx ~paint:knob_reverse;
        stroke ctx;
        fill ctx;
end
