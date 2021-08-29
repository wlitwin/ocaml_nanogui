open! Widget

open Float

class scrollpanel parent = object(self)
    inherit widget parent as super

    val mutable vscroll : float = 0.
    val mutable hscroll : float = 0.
    val mutable childPreferredHeight : float = 0.
    val mutable childPreferredWidth : float = 0.
    val mutable updateLayout : bool = false
    val mutable overflow_x : bool = true
    val mutable overflow_y : bool = true
    val mutable scrollBarSize : float = 12.

    method vertScroll = vscroll
    method setVertScroll s = vscroll <- s

    method horizScroll = hscroll
    method setHorizScroll s = hscroll <- s

    method overflowX = overflow_x
    method setOverflowX o = overflow_x <- o

    method overflowY = overflow_y
    method setOverflowY o = overflow_y <- o

    method hasOverflowX = childPreferredWidth > size.a && overflow_x
    method hasOverflowY = childPreferredHeight > size.b && overflow_y

    method scrollBarSize = scrollBarSize
    method setScrollBarSize s = scrollBarSize <- s

    method! performLayout ctx =
        super#performLayout ctx;

        if (V.length children > 1) then (
            failwith "Scroll panels can only have one child"
        );

        if not V.(is_empty children) then (
            let child = V.get children 0 in
            let ch_size = child#preferredSize ctx in
            childPreferredWidth <- ch_size.a;
            childPreferredHeight <- ch_size.b;

            let open Float in
            if childPreferredWidth > size.a 
                && childPreferredHeight > size.b 
                && overflow_x && overflow_y
            then (
                child#setPosition (Vec2.mk 
                    (~-.hscroll*(childPreferredWidth - size.a)) 
                    (~-.vscroll*(childPreferredHeight - size.b)));
                child#setSize (Vec2.mk (childPreferredWidth -. scrollBarSize) (childPreferredHeight -. scrollBarSize));
            ) else if childPreferredHeight > size.b && overflow_y then (
                child#setPosition (Vec2.mk 0. (~-.vscroll*(childPreferredHeight - size.b)));
                child#setSize (Vec2.mk (size.a-scrollBarSize) childPreferredHeight);
            ) else if childPreferredWidth > size.a && overflow_x then (
                child#setPosition (Vec2.mk (~-.hscroll*(childPreferredWidth - size.a)) 0.);
                child#setSize (Vec2.mk childPreferredWidth (size.b-scrollBarSize));
            ) else (
                child#setPosition Vec2.zero;
                child#setSize ch_size;
                vscroll <- 0.;
                hscroll <- 0.;
            );
            child#performLayout ctx;
        )

    method! preferredSize ctx =
        if V.is_empty children then (
            Vec2.zero
        ) else (
            let ch_pref = V.(get children 0)#preferredSize ctx in
            Vec2.(ch_pref + mk1 scrollBarSize)
        )

    method private calcScroll (rel : Vec2.t) =
        let width, height = size.a, size.b in
        let scrollh = height * (min 1. (height / childPreferredHeight)) in
        let scrollv = width * (min 1. (width / childPreferredWidth)) in
        let extra_h = childPreferredWidth > width && overflow_x in
        let extra_w = childPreferredHeight > height && overflow_y in
        let offset_w = if extra_w then scrollBarSize else 0. in
        let offset_h = if extra_h then scrollBarSize else 0. in
        vscroll <- (max 0. (min 1. (vscroll - rel.b / (height - offset_h - scrollh))));
        hscroll <- (max 0. (min 1. (hscroll - rel.a / (width - offset_w - scrollv))));
        if not overflow_x || childPreferredWidth <= width then (hscroll <- 0.);
        if not overflow_y || childPreferredHeight <= height then (vscroll <- 0.);
        updateLayout <- true;
        if V.length children != 0 then (
            let child = V.get children 0 in
            let scroll_v = ~-.vscroll*(childPreferredHeight + offset_h - height) in
            let scroll_h = ~-.hscroll*(childPreferredWidth + offset_w - width) in
            child#setPosition (Vec2.mk scroll_h scroll_v);
        );
        true

    method! mouseButtonEvent ~pos:_ _button _action _mods =
        Caml.print_endline "MB event";
        true

    method! mouseDragEvent ~pos ~rel button mods =
        if not V.(is_empty children) && childPreferredHeight > size.b then (
            (* TODO - probably should make scrollbars their own
             *        controls instead then dont have to do special masking
             *)
            let rel = 
                let pos = Vec2.(pos - position) in
                if pos.a > size.a && pos.b > size.b then Vec2.inv rel
                else if pos.a > size.a then Vec2.inv_y rel
                else if pos.b > size.b then Vec2.inv_x rel
                else rel
            in
            self#calcScroll Vec2.(inv rel);
        ) else (
            super#mouseDragEvent ~pos ~rel button mods
        )

    method! scrollEvent ~pos ~rel =
        if not V.(is_empty children) 
            && ((childPreferredHeight > size.b && overflow_y)
                || (childPreferredWidth > size.a && overflow_x))
        then (
            self#calcScroll rel;
        ) else (
            super#scrollEvent ~pos ~rel
        )

    (* TODO - ensureVisible rect *)

    method! draw ctx =
        if not V.(is_empty children) then (
            let child = V.get children 0 in
            if updateLayout then (
                let ch_pref = child#preferredSize ctx in
                childPreferredWidth <- ch_pref.a;
                childPreferredHeight <- ch_pref.b;
                child#performLayout ctx;
                updateLayout <- false;
            );

            let has_overflow_x = childPreferredWidth > size.a && overflow_x in
            let has_overflow_y = childPreferredHeight > size.b && overflow_y in

            let open Gv in
            if child#visible then (
                save ctx;
                let sh = if has_overflow_x then size.b - scrollBarSize else size.b in
                let sw = if has_overflow_y then size.a - scrollBarSize else size.a in
                Gv.Scissor.intersect ctx ~x:0. ~y:0. ~w:sw ~h:sh;
                
                let cpos = child#position in
                Transform.translate ctx ~x:cpos.a ~y:cpos.b;

                child#draw ctx;
                restore ctx;
            );

            let extra =
                if has_overflow_x && has_overflow_y 
                then scrollBarSize
                else 0.
            in

            if has_overflow_y then (
                let x = size.a - scrollBarSize in
                let y = 4. in
                let w = 8. in
                let h = size.b - 8. - extra in
                let paint = Paint.box_gradient ctx 
                    ~x:(x + 1.) ~y:(y + 1.) ~w ~h ~r:3. ~f:4.
                    ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:32) 
                    ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:92)
                in
                Path.begin_ ctx;
                Path.rounded_rect ctx ~x ~y ~w ~h ~r:3.;
                set_fill_paint ctx ~paint;
                fill ctx;

                let height = self#height in
                let scrollh = height * (min 1. (height / childPreferredHeight)) in
                let paint = Paint.box_gradient ctx 
                    ~x:(x - 1.) ~y:(y + (h - scrollh)*vscroll - 1.) ~w ~h:scrollh
                    ~r:3. ~f:4. 
                    ~icol:(Color.rgba ~r:220 ~g:220 ~b:220 ~a:100) 
                    ~ocol:(Color.rgba ~r:128 ~g:128 ~b:128 ~a:100)
                in
                Path.begin_ ctx;
                Path.rounded_rect ctx ~x:(x + 1.) ~y:(y + 1. + (h - scrollh)*vscroll) ~w:(w-2.) ~h:(scrollh - 2.) ~r:2.;
                set_fill_paint ctx ~paint;
                fill ctx;
            );

            if has_overflow_x then ( 
                let x = 4. in
                let y = size.b - scrollBarSize in
                let w = size.a - 8. - extra in
                let h = 8. in
                let paint = Paint.box_gradient ctx ~x:(x + 1.) ~y:(y + 1.) ~w ~h ~r:3. ~f:4.
                    ~icol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:32) 
                    ~ocol:(Color.rgba ~r:0 ~g:0 ~b:0 ~a:92)
                in
                Path.begin_ ctx;
                Path.rounded_rect ctx ~x ~y ~w ~h ~r:3.;
                set_fill_paint ctx ~paint;
                fill ctx;

                let width = self#width in
                let scrollv = width * (min 1. (width / childPreferredWidth)) in
                let paint = Paint.box_gradient ctx 
                    ~x:(x + (w - scrollv)*hscroll - 1.) ~y:(y - 1.) ~w:scrollv ~h
                    ~r:3. ~f:4. 
                    ~icol:(Color.rgba ~r:220 ~g:220 ~b:220 ~a:100) 
                    ~ocol:(Color.rgba ~r:128 ~g:128 ~b:128 ~a:100)
                in
                Path.begin_ ctx;
                Path.rounded_rect ctx ~x:(x + 1. + (w - scrollv)*hscroll) ~y:(y+1.) ~w:(scrollv - 2.) ~h:(h-2.) ~r:2.;
                set_fill_paint ctx ~paint;
                fill ctx;
            )
        )
end
