open! Widget

type side = Top | Bottom | Left | Right
 
class popup (anchor : widget) = object(self)
    inherit widget None as super
    
    val mutable anchorPos = Vec2.zero
    val mutable anchorHeight = 0.
    val mutable side = Right
    val mutable needsLayout = true
    val mutable draggable = true
    val mutable anchor : widget = anchor

    method anchor = anchor
    method setAnchor a = anchor <- a

    method draggable = draggable
    method setDraggable d = draggable <- d

    method anchorPos = anchorPos
    method setAnchorPos a = anchorPos <- a;

    method anchorHeight = anchorHeight
    method setAnchorHeight a = 
        anchorHeight <- a;
        needsLayout <- true

    method! focusEvent ~focus =
        let res = super#focusEvent ~focus in
        (* Put this at the end of the screen's children 
         * so we draw on top
         *)
        begin match parent with
        | None -> ()
        | Some p -> 
            p#removeChildWidget (self :> widget);
            p#addChildWidget (self :> widget);
        end;
        res

    method side = side
    method setSide s = 
        side <- s;
        needsLayout <- true

    method! setVisible v =
        super#setVisible v;
        (* Put ourselves first on the popup list *)
        match anchor#screen with
        | None -> ()
        | Some sc ->
            if v then (
                sc#addChildWidget (self :> widget);
                needsLayout <- true;
            )
            else sc#removeChildWidget (self :> widget)

    initializer
        visible <- false;

    method! performLayout ctx =
        size <- self#fixedOrPreferredSize ctx;

        begin 
            let p = anchor in
            match side with
            | Left -> anchorPos <- Vec2.mk (~-.(size.Vec2.a +. anchorHeight)) ((p#size.b -. size.b)*.0.5);
            | Right -> anchorPos <- Vec2.mk (p#size.a +. anchorHeight) ((p#size.b -. size.b)*.0.5);
            | Top -> anchorPos <- Vec2.mk (~-.(size.a-.p#size.a)*.0.5) (~-.(size.b+.anchorHeight));
            | Bottom -> anchorPos <- Vec2.mk (~-.(size.a-.p#size.a)*.0.5) (p#size.b +. anchorHeight);
        end;

        position <- Vec2.(anchor#absolutePosition + anchorPos);

        if Option.is_some layout || V.length children > 1 then (
            super#performLayout ctx;
        ) else (
            let ch = V.get children 0 in
            ch#setPosition Vec2.zero;
            ch#setSize size;
            ch#performLayout ctx;
        );

    method! preferredSize ctx =
        if Option.is_some layout || V.length children > 1 then (
            super#preferredSize ctx
        ) else (
            V.(get children 0)#preferredSize ctx
        )

    method refreshRelativePlacement ctx =
        if needsLayout then (
            self#performLayout ctx;
            needsLayout <- false;
        );

    method! mouseButtonEvent ~pos:_ _button _down _mods =
        true

    method! mouseDragEvent ~pos ~rel btn mods =
        if draggable then (
            position <- Vec2.(position + rel);
            true
        ) else (
            super#mouseDragEvent ~pos ~rel btn mods
        )

    method! draw ctx =
        (* We jump because the parent tried to move to our position,
         * but we have not updated it yet... because we were unable
         * to run layout because lack of context. Should probably
         * make the context more pervasive to do layout other times
         *)
        if visible && not needsLayout then (

            let ds = theme#windowDropShadowSize in
            let cr = theme#windowCornerRadius in

            let open Float in
            let open Nanovg in
            save ctx;
            reset_scissor ctx;

            let shadow_paint = box_gradient ctx 0. 0. size.a size.b (cr*2.) (ds*2.)
                theme#dropShadow theme#transparent
            in

            (* Drop shadow *)
            begin_path ctx;
            rect ctx ~-ds ~-ds (size.a+2.*ds) (size.b+2.*ds);
            rounded_rect ctx 0. 0. size.a size.b cr;
            path_winding ctx Solidity.hole;
            fill_paint ctx shadow_paint;
            fill ctx;

            (* Window *)
            begin_path ctx;
            rounded_rect ctx 0. 0. size.a size.b cr;

            let sign, base = match side with
                     | Left -> 1., Vec2.mk size.a anchorHeight
                     | _ -> -1., Vec2.mk 0. anchorHeight
            in
            
            move_to ctx (base.a+15.*sign) base.b;
            move_to ctx (base.a-sign) (base.b-15.);
            move_to ctx (base.a-sign) (base.b+15.);
            fill_color ctx theme#windowPopup;
            fill ctx;

            super#draw ctx;
            restore ctx;
        ) else (
            self#refreshRelativePlacement ctx;
        )
end
