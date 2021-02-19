open! Widget

class stackedwidget parent = object(self)
    inherit widget parent as super

    val mutable selectedIndex : int option = None

    method setSelectedIndex idx =
        assert (idx < self#childCount);
        begin match selectedIndex with
        | Some v -> V.(get children v)#setVisible false
        | None -> ()
        end;
        selectedIndex <- Some idx;
        V.(get children idx)#setVisible true

    method selectedIndex = selectedIndex

    method! performLayout ctx =
        (* Why not just layout the current child? *)
        V.iter (fun child ->
            child#setPosition Vec2.zero;
            child#setSize size;
            child#performLayout ctx;
        ) children
    
    method! preferredSize ctx =
        V.fold (fun size child ->
            Vec2.max size (child#preferredSize ctx)
        ) Vec2.zero children 

    method! addChild idx widget =
        begin match selectedIndex with 
        | Some v -> V.(get children v)#setVisible false
        | None -> ()
        end;
        super#addChild idx widget;
        widget#setVisible true;
        self#setSelectedIndex idx
end

