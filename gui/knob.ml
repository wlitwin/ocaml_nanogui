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
        let open Nanovg in
        let open Float in

        let center = Vec2.(size * 0.5) in
        let radius = (min size.a size.b) * 0.45 in
        let radius_min = radius - radius*0.3 in

        let _bg = radial_gradient ctx center.a center.b 
            radius_min radius (rgba 255 255 255 64) (rgba 16 16 16 64)
        in

        let make_arc start end_ =
            begin_path ctx;
            arc ctx center.a center.b radius_min start end_  Winding.cw;
            arc ctx center.a center.b radius end_ start  Winding.ccw;
            close_path ctx;
        in

        (* Draw arc *)
        let start = 0.75*pi in
        let end_ = 2.25*pi in
        fill_paint ctx _bg;
        make_arc start end_;
        stroke_color ctx theme#borderDark;
        stroke ctx;
        fill ctx;

        let angle = lerp 0.75 2.25 ~t:((value - minValue) / (maxValue - minValue)) in
        let angle = angle*pi in

        (* Draw filled portion of arc *)
        if value > 0. then (
            make_arc start angle;
            fill_color ctx (rgba 110 204 236 255);
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
        let arrow_bg = linear_gradient ctx left.a left.b right.a right.b (rgba 16 16 16 255) (rgba 128 128 128 255) in
        begin_path ctx;
        stroke_width ctx 2.;
        stroke_color ctx (rgba 128 128 128 255);
        fill_paint ctx arrow_bg;
        (*fill_color ctx (rgba 96 96 96 255);*)
        move_to ctx top.a top.b;
        line_to ctx right.a right.b;
        line_to ctx bot.a bot.b;
        line_to ctx left.a left.b;
        close_path ctx;
        stroke ctx;
        fill ctx;
end
