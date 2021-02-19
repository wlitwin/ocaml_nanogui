open! Widget

class vboxlayout = object
    method preferredSize (ctx : graphics_context) (widget : widget) =
        let max_x = ref 0. in
        let y_total = ref 0. in
        V.iter (fun child ->
            let size = child#fixedOrPreferredSize ctx in
            max_x := Float.max !max_x size.Vec2.a;
            y_total := !y_total +. size.Vec2.b;
        ) widget#children;
        Vec2.mk !max_x !y_total

    method performLayout (ctx : graphics_context) (widget : widget) =
        let wsize = widget#size in
        let y_pos = ref 0. in
        V.iter (fun child ->
            child#setPosition Vec2.(mk 0. !y_pos);
            let size = child#fixedOrPreferredSize ctx in
            Printf.printf "Y %.2f SIZE %.2f\n%!" !y_pos size.Vec2.b;
            y_pos := !y_pos +. size.Vec2.b;
            child#setSize Vec2.(mk wsize.a size.b);
            child#performLayout ctx;
        ) widget#children
end
