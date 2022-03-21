open! Widget

module Region : sig
    type t
    val none : t
    val inner_triangle : t
    val outer_circle  : t
    val both : t
    val add : t -> t -> t
    val clear : t -> t -> t
    val is_set : t -> t -> bool
    val is_none : t -> bool
    val equal : t -> t -> bool
end = struct
    type t = int
    let none = 0xf
    let inner_triangle = 0x1
    let outer_circle = 0x2
    let both = inner_triangle lor outer_circle

    let is_none t = (t = 0)
    let equal = (=)
    let add t v =
        if t = none then v
        else (t lor v)

    let clear t v =
        if t = none then 0
        else t land (lnot v)

    let is_set t v =
        if t = none then false
        else (t land v) > 0

    let%expect_test _ =
        Printf.printf "%x = f\n" (add none none);
        Printf.printf "%x = 1\n" (add none inner_triangle);
        Printf.printf "%x = 2\n" (add none outer_circle);
        Printf.printf "%x = 3\n" (add none both);
        Printf.printf "%x = 2\n" (clear both inner_triangle);
        Printf.printf "%x = 1\n" (clear both outer_circle);
        Printf.printf "%x = 0\n" (clear (clear both outer_circle) inner_triangle);
        Printf.printf "%x = 0\n" (clear both none);
      [%expect {|
        f = f
        1 = 1
        2 = 2
        3 = 3
        2 = 2
        1 = 1
        0 = 0
        0 = 0 |}]
    ;;

    let%expect_test _ =
        Printf.printf "false = %b\n" (is_set none none);
        Printf.printf "false = %b\n" (is_set none outer_circle);
        Printf.printf "false = %b\n" (is_set none inner_triangle);
        Printf.printf "true = %b\n" (is_set both inner_triangle);
        Printf.printf "true = %b\n" (is_set both outer_circle);
        Printf.printf "true = %b\n" (is_set outer_circle outer_circle);
        Printf.printf "true = %b\n" (is_set inner_triangle inner_triangle);
        Printf.printf "false = %b\n" (is_set outer_circle inner_triangle);
        Printf.printf "false = %b\n" (is_set inner_triangle outer_circle);
      [%expect {|
        false = false
        false = false
        false = false
        true = true
        true = true
        true = true
        true = true
        false = false
        false = false |}]
    ;;
end

type region = None
            | InnerTriangle
            | OuterCircle
            | Both

let bary_test _hue _x _y _ax _ay _bx _by _r =
    (* TODO - finish triangle calcs *)
    let l0 = 1.
    and l1 = 1.
    and l2 = 1. in
    let _triangle_test =
        l0 >= 0. && l0 <= 1. && l1 >= 0. && l1 <= 1. && l2 >= 0. && l2 <= 1. in
    false, 0., 0., 0.
;;

let hue_to_rgb h =
    let open Float in
    let s = 1. and v = 1. in
    let h = if h < 0. then h + 1. else h in
    
    let i = round_down (h*6.) |> to_int in
    let f = h * 6. - (of_int i) in
    let p = v * (1. - s) in
    let q = v * (1. - f*s) in
    let t = v * (1. - (1. - f) * s) in
    let i = i mod 6 in
    match i with
    | 0 -> v, t, p
    | 1 -> q, v, p
    | 2 -> p, v, t
    | 3 -> p, q, v
    | 4 -> t, p, v
    | 5 -> v, p, q
    | _ -> failwith "hue_to_rgb"
;;

class colorwheel parent color = object(self)
   inherit widget parent as super

   val mutable hue = 0.
   val mutable white = 0.5
   val mutable black = 0.2
   val mutable dragRegion : Region.t = Region.none
   val mutable callback = (fun _ -> ())

   initializer
       self#setColor color

   method callback = callback
   method setCallback c = callback <- c

   method! preferredSize _ctx =
       Vec2.mk1 100.

   method! draw vg =
       let open Gv in
       let open Float in
       super#draw vg;

       if visible then (

           let x, y, w, h = 0., 0., size.a, size.b in
           save vg;

            let cx = x + w*0.5 in
            let cy = y + h*0.5 in
            let r1 = (if w < h then w else h) * 0.5 - 5. in
            let r0 = r1*0.75 in

            let aeps = 0.5 / r1 in (* half a pixel arc length in radians *)
            
            for i=0 to 5 do
                let i_f = of_int i in
                let a0 = i_f / 6. * pi * 2. - aeps in
                let a1 = (i_f+1.) / 6. * pi * 2. + aeps in
                Path.begin_ vg;
                Path.arc vg ~cx ~cy ~r:r0 ~a0 ~a1 ~dir:Winding.CW;
                Path.arc vg ~cx ~cy ~r:r1 ~a0:a1 ~a1:a0 ~dir:Winding.CCW;
                Path.close vg;

                let ax = cx + cos a0 * (r0+r1)*0.5 in
                let ay = cy + sin a0 * (r0+r1)*0.5 in
                let bx = cx + cos a1 * (r0+r1)*0.5 in
                let by = cy + sin a1 * (r0+r1)*0.5 in
                let paint = Paint.linear_gradient vg ~sx:ax ~sy:ay ~ex:bx ~ey:by 
                    ~icol:(Color.hsla ~h:(a0/(2.*pi)) ~s:1. ~l:0.55 ~a:255)
                   ~ocol:(Color.hsla ~h:(a1/(2.*pi)) ~s:1. ~l:0.55 ~a:255) 
                in
                set_fill_paint vg ~paint;
                fill vg;
            done;

            Path.begin_ vg;
            Path.circle vg ~cx ~cy ~r:(r0-0.5);
            Path.circle vg ~cx ~cy ~r:(r1+0.5);
            set_stroke_color vg ~color:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:64);
            set_stroke_width vg ~width:1.;
            stroke vg;

            (* Selector *)
            save vg;
            Transform.translate vg ~x:cx ~y:cy;
            Transform.rotate vg ~angle:(hue*2.*pi);

            (* Marker on *)
            let u = max (r1/50.) 1.5 in
            let u = min u 4. in
            set_stroke_width vg ~width:u;
            Path.begin_ vg;
            Path.rect vg ~x:(r0-1.) ~y:(~-2.*u) ~w:(r1-r0+2.) ~h:(4.*u);
            set_stroke_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:192);
            stroke vg;

            let paint = Paint.box_gradient vg 
                ~x:(r0-3.) 
                ~y:~-.5. 
                ~w:(r1-r0+6.) 
                ~h:10. ~r:2. ~f:4. 
                ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:128) 
                ~ocol:(Color.transparent)
            in
            Path.begin_ vg;
            Path.rect vg ~x:(r0-2.-10.) ~y:(~-.4.-10.) ~w:(r1-r0+4.+20.) ~h:(8.+20.);
            Path.rect vg ~x:(r0-2.) ~y:~-.4. ~w:(r1-r0+4.) ~h:8.;
            Path.winding vg ~winding:Winding.CW; (* hole *)
            set_fill_paint vg ~paint;
            fill vg;

            (* Center triangle *)
            let v = 120. / 180. * pi in
            let r = r0 - 6. in
            let ax = cos v * r in
            let ay = sin v * r in
            let bx = cos ~-.v * r in
            let by = sin ~-.v * r in
            Path.begin_ vg;
            Path.move_to vg ~x:r ~y:0.;
            Path.line_to vg ~x:ax ~y:ay;
            Path.line_to vg ~x:bx ~y:by;
            Path.close vg;
            
            let paint = Paint.linear_gradient vg ~sx:r ~sy:0. ~ex:ax ~ey:ay 
                ~icol:(Color.hsla ~h:hue ~s:1. ~l:0.5 ~a:255) 
                ~ocol:(Color.white) 
            in
            set_fill_paint vg ~paint;
            fill vg;

            let paint = Paint.linear_gradient vg 
                ~sx:((r+ax)*0.5) ~sy:((0. + ay)*0.5) ~ex:bx ~ey:by 
                ~icol:Color.transparent ~ocol:Color.black
            in
            set_fill_paint vg ~paint;
            fill vg;
            set_stroke_color vg ~color:Color.(rgba ~r:0 ~g:0 ~b:0 ~a:64);
            stroke vg;

            (* Select circle on triangle *)
            let sx = r*(1. - white - black) + ax*white + bx*black in
            let sy = ax*white + by*black in
            set_stroke_width vg ~width:u;
            Path.begin_ vg;
            Path.circle vg ~cx:sx ~cy:sy ~r:(2.*u);
            set_stroke_color vg ~color:(Color.rgba ~r:255 ~g:255 ~b:255 ~a:192);
            stroke vg;
            restore vg;
            restore vg;
       )

    method adjustPosition (p : Vec2.t) (consideredRegions : Region.t) : Region.t =
        let open Float in
        let x = p.a - position.a in
        let y = p.b - position.b in
        let w = size.a in
        let h = size.b in

        let cx = w*0.5 in
        let cy = h*0.5 in
        let r1 = (if w < h then w else h)*0.5 - 5. in
        let r0 = r1*0.75 in

        let x = x - cx in
        let y = y - cy in

        let mr = sqrt (x*x + y*y) in
        
        if (Region.is_set consideredRegions Region.outer_circle)
            && ((mr >= r0 && mr <= r1) || Region.(equal consideredRegions outer_circle)) then (
                hue <- atan (y / x);
                if x < 0. then (
                    hue <- hue + pi;
                );
                hue <- hue / (2.*pi);
                
                callback self#color;
                Region.outer_circle
        ) else (
            let r = r0 - 6. in
            let v = 120. / 180. * pi in
            let ax = cos v * r in
            let ay = sin v * r in
            let bx = cos ~-.v * r in
            let by = sin ~-.v * r in

            let triangle_test, l0, l1, l2 = bary_test hue x y ax ay bx by r in

            if Region.(is_set consideredRegions inner_triangle) &&
                (triangle_test || Region.(equal consideredRegions inner_triangle)) then (
                    let l0 = min (max 0. l0) 1. in
                    let l1 = min (max 0. l1) 1. in
                    let l2 = min (max 0. l2) 1. in
                    let sum = l0 + l1 + l2 in
                    let l0 = l0 / sum
                    and l1 = l1 / sum in
                    white <- l0;
                    black <- l1;
                    callback self#color;
                    Region.inner_triangle
            ) else 
                Region.none
        )


    method! mouseDragEvent ~pos ~rel:_ _button _mods =
        Region.is_none (self#adjustPosition pos dragRegion) |> not

    method! mouseButtonEvent ~pos button down _mods =
        if not enabled || button <> Mouse.button_left then (
            false
        ) else if down then (
            dragRegion <- self#adjustPosition pos Region.both;
            not (Region.is_none dragRegion)
        ) else (
            dragRegion <- Region.none;
            true
        )

    method color =
        let open Float in
        let (r, g, b), a = hue_to_rgb hue, 1. in
        let (rb, gb, bb, ab) = 0., 0., 0., 1. in
        let (rw, gw, bw, aw) = 1., 1., 1., 1. in

        let scale = 1. -. white -. black in
        let rv = r*scale + (rb * black) + (rw * white) in
        let gv = g*scale + (gb * black) + (gw * white) in
        let bv = b*scale + (bb * black) + (bw * white) in
        let av = a*scale + (ab * black) + (aw * white) in
        rv, gv, bv, av

    method setColor (r,g,b) =
        let open Float in
        let max = max r (max g b) in
        let min = min r (min g b) in
        let l = (min + max) * 0.5 in

        if max = min then (
            hue <- 0.;
            black <- 1. - l;
            white <- l;
        ) else (
            let d = max - min in
            let h =
                if max = r then (
                    (g - b) / d + (if g < b then 6. else 0.)
                ) else if max = g then (
                    (b - r) / d + 2.
                ) else (
                    (r - g) / d + 4.
                )
            in
            let h = h / 6. in
            hue <- h;

            (*
            white <- min;
            black <- 1. - max;
            *)
        )
end
