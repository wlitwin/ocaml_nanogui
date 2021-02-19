open Widget

class fullLayout = object
    method preferredSize (ctx : graphics_context) (widget : widget) : Vec2.t =
        widget#fixedOrPreferredSize ctx

    method performLayout (ctx : graphics_context) (widget : widget) : unit =
        let size = widget#size in
        V.iter (fun child ->
            child#setPosition Vec2.zero;
            child#setSize size;
            child#performLayout ctx;
        ) widget#children
end
