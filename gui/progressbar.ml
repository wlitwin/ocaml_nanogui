open! Widget

class progressbar parent = object
    inherit widget parent as super

    val mutable value : float = 0.

    method value = value
    method setValue v =
        value <- Float.clamp_exn ~min:0. ~max:1. v

    method! preferredSize _ctx =
        Vec2.mk 70. 12.

    method! draw ctx =
        super#draw ctx;
        let open Nanovg in
        let open Float in

        let paint = 
            box_gradient ctx 1. 1. (size.a-2.) size.b 3. 4.
            (rgba 0 0 0 32) (rgba 0 0 0 92)
        in
        begin_path ctx;
        rounded_rect ctx 0. 0. size.a size.b 3.;
        fill_paint ctx paint;
        fill ctx;

        let bar_pos = (size.a - 2.) * value in

        let paint = box_gradient ctx 0. 0. (bar_pos+1.5) (size.b-1.) 3. 4.
            (rgba 220 220 220 100) (rgba 128 128 128 100)
        in

        begin_path ctx;
        rounded_rect ctx 1. 1. bar_pos (size.b-2.) 3.;
        fill_paint ctx paint;
        fill ctx;
end
