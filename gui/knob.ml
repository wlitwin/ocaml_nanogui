open Slider

class knob parent ~min ~max ~value = object(_self)
    inherit slider parent ~min ~max ~value as _super

    method! preferredSize _ctx =
        Vec2.mk 75. 75.

    method! private calcValue (pos : Vec2.t) =
        let open Float in
        let pos = Vec2.(pos - position) in
        let c = Vec2.(size * 0.5) in
        let p = Vec2.(c - pos) in
        let t = 
            let t = atan2 p.a ~-.(p.b) in
            if t < 0. 
            then (
                t + (2.*pi) 
            )
            else t
        in
        let t = (t-0.75) / 4.5 in
        let t = clamp_exn ~min:0. ~max:1. t in
        Float.lerp minValue maxValue ~t

    method! draw ctx =
        let open Widget.Gv in
        let open Float in

        let center = Vec2.(size * 0.5) in
        let radius = (min size.a size.b) * 0.45 in
        let radius_min = radius - radius*0.3 in

        let bg = Paint.radial_gradient ctx ~cx:center.a ~cy:center.b 
            ~in_radius:radius_min ~out_radius:radius 
            ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:32) 
            ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:32)
        in

        let make_arc start end_ =
            Path.begin_ ctx;
            Path.arc ctx ~cx:center.a ~cy:center.b ~r:radius_min ~a0:start ~a1:end_  ~dir:Winding.CW;
            Path.arc ctx ~cx:center.a ~cy:center.b ~r:radius ~a0:end_ ~a1:start  ~dir:Winding.CCW;
            Path.close ctx;
        in

        (* Draw arc *)
        let start = 0.75*pi in
        let end_ = 2.25*pi in
        set_fill_paint ctx ~paint:bg;
        make_arc start end_;
        set_stroke_color ctx ~color:theme#borderDark;
        stroke ctx;
        fill ctx;

        let angle = lerp 0.75 2.25 ~t:((value - minValue) / (maxValue - minValue)) in
        let angle = angle*pi in

        (* Draw filled portion of arc *)
        if value > 0. then (
            make_arc start angle;
            set_fill_color ctx ~color:(Color.rgba ~r:220 ~g:220 ~b:220 ~a:100);
            fill ctx;
        );

        (* Draw arrow *)
        (* TODO - maybe using a transform is better..., faster? *)
        let angle2 = angle + 0.5*pi in
        let px = (cos angle) in 
        let py = (sin angle) in
        let px2 = cos angle2 in
        let py2 = sin angle2 in
        let width = radius_min*0.2 in
        let vup = Vec2.mk px py in
        let vright = Vec2.mk px2 py2 in
        let right = Vec2.(center + vright*width) in
        let left = Vec2.(center - vright*width) in
        let top = Vec2.(center + vup*radius_min) in
        let bot = Vec2.(center - vup*width) in
        let arrow_bg = Paint.linear_gradient ctx 
            ~sx:left.a ~sy:left.b ~ex:right.a ~ey:right.b 
            ~icol:(Color.rgba ~r:16 ~g:16 ~b:16 ~a:255) 
            ~ocol:(Color.rgba ~r:128 ~g:128 ~b:128 ~a:255) 
        in
        Path.begin_ ctx;
        set_stroke_width ctx ~width:2.;
        set_stroke_color ctx ~color:(Color.rgba ~r:128 ~g:128 ~b:128 ~a:255);
        set_fill_paint ctx ~paint:arrow_bg;
        (*fill_color ctx (rgba 96 96 96 255);*)
        Path.move_to ctx ~x:top.a ~y:top.b;
        Path.line_to ctx ~x:right.a ~y:right.b;
        Path.line_to ctx ~x:bot.a ~y:bot.b;
        Path.line_to ctx ~x:left.a ~y:left.b;
        Path.close ctx;
        stroke ctx;
        fill ctx;
end
