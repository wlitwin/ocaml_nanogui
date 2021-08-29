open! Widget

open Float

class vscrollpanel parent = object(self)
    inherit widget parent as super

    val mutable scroll : float = 0.
    val mutable childPreferredHeight : float = 0.
    val mutable updateLayout : bool = false

    method scroll = scroll
    method setScroll s = scroll <- s

    method! performLayout ctx =
        super#performLayout ctx;

        if (V.length children > 1) then (
            failwith "Scroll panels can only have one child"
        );

        if not V.(is_empty children) then (
            let child = V.get children 0 in
            childPreferredHeight <- (child#preferredSize ctx).b;

            if childPreferredHeight > size.b then (
                child#setPosition (Vec2.mk 0. (~-.scroll*(childPreferredHeight - size.b)));
                child#setSize (Vec2.mk (size.a-12.) childPreferredHeight);
            ) else (
                child#setPosition Vec2.zero;
                child#setSize size;
                scroll <- 0.;
            );
            child#performLayout ctx;
        )

    method! preferredSize ctx =
        if V.is_empty children then 
            Vec2.zero
        else (
            Vec2.(V.(get children 0)#preferredSize ctx + mk 12. 0.)
        )

    method private calcScroll (rel : Vec2.t) =
        let height = self#height in
        let scrollh = height * (min 1. (height / childPreferredHeight)) in
        scroll <- (max 0. (min 1. (scroll - rel.b / (size.b - 8. - scrollh))));
        updateLayout <- true;
        true

    method! mouseDragEvent ~pos ~rel button mods =
        if not V.(is_empty children) && childPreferredHeight > size.b then (
            self#calcScroll rel;
        ) else (
            super#mouseDragEvent ~pos ~rel button mods
        )

    method! scrollEvent ~pos ~rel =
        if not V.(is_empty children) && childPreferredHeight > size.b then (
            self#calcScroll rel;
        ) else (
            super#scrollEvent ~pos ~rel
        )

    method! draw ctx =
        if not V.(is_empty children) then (
            let child = V.get children 0 in
            child#setPosition (Vec2.mk 0. (~-.scroll*(childPreferredHeight - size.b)));
            childPreferredHeight <- (child#preferredSize ctx).b;
            if updateLayout then (
                child#performLayout ctx;
                updateLayout <- false;
            );

            let open Gv in
            if child#visible then (
                save ctx;
                Scissor.intersect ctx ~x:0. ~y:0. ~w:size.a ~h:size.b;
                
                let cpos = child#position in
                Transform.translate ctx ~x:cpos.a ~y:cpos.b;

                child#draw ctx;
                restore ctx;
            );

            if childPreferredHeight > size.b then (
                let paint = Paint.box_gradient ctx 
                    ~x:(size.a - 12. + 1.) ~y:(4. + 1.) ~w:8. ~h:(size.b - 8.) ~r:3. ~f:4.
                    ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:32) ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:92)
                in
                Path.begin_ ctx;
                Path.rounded_rect ctx ~x:(size.a - 12.) ~y:4. ~w:8. ~h:(size.b - 8.) ~r:3.;
                set_fill_paint ctx ~paint;
                fill ctx;

                let height = self#height in
                let scrollh = height * (min 1. (height / childPreferredHeight)) in
                let paint = Paint.box_gradient ctx 
                    ~x:(size.a - 12. - 1.) ~y:(4. + (size.b - 8. - scrollh)*scroll - 1.) ~w:8. ~h:scrollh
                    ~r:3. ~f:4. 
                    ~icol:(Color.rgba ~r:220 ~g:220 ~b:220 ~a:100) 
                    ~ocol:(Color.rgba ~r:128 ~g:128 ~b:128 ~a:100)
                in
                Path.begin_ ctx;
                Path.rounded_rect ctx 
                    ~x:(size.a - 12. + 1.)
                    ~y:(4. + 1. + (size.b - 8. - scrollh)*scroll)
                    ~w:6. 
                    ~h:(scrollh - 2.) ~r:2.;
                set_fill_paint ctx ~paint;
                fill ctx;
            )
        )
end
