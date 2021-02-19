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

            let open Nanovg in
            if child#visible then (
                save ctx;
                intersect_scissor ctx 0. 0. size.a size.b;
                
                let cpos = child#position in
                translate ctx cpos.a cpos.b;

                child#draw ctx;
                restore ctx;
            );

            if childPreferredHeight > size.b then (
                let paint = box_gradient ctx (size.a - 12. + 1.) (4. + 1.) 8. (size.b - 8.) 3. 4.
                    (rgba 0 0 0 32) (rgba 0 0 0 92)
                in
                begin_path ctx;
                rounded_rect ctx (size.a - 12.) 4. 8. (size.b - 8.) 3.;
                fill_paint ctx paint;
                fill ctx;

                let height = self#height in
                let scrollh = height * (min 1. (height / childPreferredHeight)) in
                let paint = box_gradient ctx (size.a - 12. - 1.) (4. + (size.b - 8. - scrollh)*scroll - 1.) 8. scrollh
                    3. 4. (rgba 220 220 220 100) (rgba 128 128 128 100)
                in
                begin_path ctx;
                rounded_rect ctx (size.a - 12. + 1.) (4. + 1. + (size.b - 8. - scrollh)*scroll) 6. (scrollh - 2.) 2.;
                fill_paint ctx paint;
                fill ctx;
            )
        )
end
