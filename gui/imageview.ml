open Widget

class imageview parent img = object(self)
    inherit widget parent

    val mutable image : Gv.Image.image = img

    method image = image

    method private imageSize ctx =
        let w, h = Gv.Image.size ctx image in
        let w = float w in
        let h = float h in
        w, h

    method! preferredSize ctx =
        match fixed_size with
        | Some size -> size
        | None ->
            let w, h = self#imageSize ctx in
            Vec2.mk w h

    method! draw ctx =
        let open Gv in

        let paint = Paint.image_pattern ctx 
            ~cx:0. ~cy:0. ~w:size.a ~h:size.b ~angle:0. ~image:image ~alpha:1.
        in

        Path.begin_ ctx;
        Path.rect ctx ~x:0. ~y:0. ~w:size.a ~h:size.b;
        set_fill_paint ctx ~paint;
        fill ctx;
end
