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
        let open Gv in
        let open Float in

        let paint = 
            Paint.box_gradient ctx ~x:1. ~y:1. ~w:(size.a-2.) ~h:size.b ~r:3. ~f:4.
            ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:32) 
            ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:92)
        in
        Path.begin_ ctx;
        Path.rounded_rect ctx ~x:0. ~y:0. ~w:size.a ~h:size.b ~r:3.;
        set_fill_paint ctx ~paint;
        fill ctx;

        let bar_pos = (size.a - 2.) * value in

        let paint = Paint.box_gradient ctx 
            ~x:0. ~y:0. ~w:(bar_pos+1.5) ~h:(size.b-1.) ~r:3. ~f:4.
            ~icol:(Color.rgba ~r:220 ~g:220 ~b:220 ~a:100) 
            ~ocol:(Color.rgba ~r:128 ~g:128 ~b:128 ~a:100)
        in

        Path.begin_ ctx;
        Path.rounded_rect ctx ~x:1. ~y:1. ~w:bar_pos ~h:(size.b-2.) ~r:3.;
        set_fill_paint ctx ~paint;
        fill ctx;
end
