open Widget

type orientation = Vertical | Horizontal

type alignment = Minimum | Middle | Maximum | Fill

let get_axes orientation =
    match orientation with
    | Horizontal -> Vec2.get_a, Vec2.set_a, Vec2.get_b, Vec2.set_b
    | Vertical -> Vec2.get_b, Vec2.set_b, Vec2.get_a, Vec2.set_a
;;

class boxLayout orientation alignment margin spacing = object(_self)
    val mutable orientation : orientation = orientation
    val mutable alignment : alignment = alignment
    val mutable margin : float = margin
    val mutable spacing : float = spacing

    method spacing = spacing
    method setSpacing s = spacing <- s
    method orientation = orientation
    method setOrientation o = orientation <- o
    method alignment = alignment
    method setAlignment a = alignment <- a

    method preferredSize ctx (widget : widget) =
        let size = Vec2.mk1 (2.*.margin) in
        let get_a1, set_a1, get_a2, set_a2 = get_axes orientation in
        let child_count = 
            max 0 (V.length widget#children - 1) |> float_of_int
        in

        let size = set_a1 size (get_a1 size +. child_count*.spacing) in
        V.fold (fun size child ->
            if child#visible then (
                let ch_size = child#fixedOrPreferredSize ctx in
                let size = set_a1 size ((get_a1 size) +. (get_a1 ch_size)) in
                set_a2 size Float.(max (get_a2 size) (get_a2 ch_size +. 2.*.margin))
            ) else size
        ) size widget#children

    method performLayout ctx (widget : widget_spec) =
        let containerSize = match widget#fixedSize with
                 | None -> widget#size
                 | Some fs -> fs
        in

        let get_a1, set_a1, get_a2, set_a2 = get_axes orientation in
        let position = ref margin in
        let yOffset = ref 0. in

        let add_a2 pos v =
            pos := set_a2 !pos ((get_a2 !pos) +. v)
        in

        let first = ref false in
        V.iter (fun w ->
            if w#visible then (
                if !first then ( first := false )
                else position := !position +. spacing;
            );

            let targetSize = w#fixedOrPreferredSize ctx in
            let pos = ref Vec2.(mk 0. !yOffset) in

            pos := set_a1 !pos !position;
            let targetSize =
                match alignment with
                | Minimum -> add_a2 pos margin; targetSize
                | Middle  -> add_a2 pos ((get_a2 containerSize -. get_a2 targetSize)*.0.5); targetSize
                | Maximum -> add_a2 pos (get_a2 containerSize -. get_a2 targetSize -. margin *. 2.); targetSize
                | Fill -> 
                    add_a2 pos margin;
                    set_a2 targetSize (match w#fixedSize with 
                                       | Some fs -> get_a2 fs
                                       | None -> get_a2 containerSize -. margin*.2.)

            in

            w#setPosition !pos;
            w#setSize targetSize;
            w#performLayout ctx;
            position := !position +. get_a1 targetSize;
        ) widget#children
end
