open Widget

class imageview parent img = object(self)
    inherit widget parent

    val mutable image : int = img

    method image = image

    method private imageSize ctx =
        let open Nanovg in
        let open Ctypes in
        let isize = allocate_n int ~count:2 in
        image_size ctx image isize Ctypes.(isize +@ 1);
        let w = Float.of_int !@isize in
        let h = Float.of_int !@(isize +@ 1) in
        w, h

    method! preferredSize ctx =
        match fixed_size with
        | Some size -> size
        | None ->
            let w, h = self#imageSize ctx in
            Vec2.mk w h

    method! draw ctx =
        let open Nanovg in

        let paint = image_pattern ctx 0. 0. size.a size.b 0. image 1. in

        begin_path ctx;
        rect ctx 0. 0. size.a size.b;
        fill_paint ctx paint;
        fill ctx;
end
