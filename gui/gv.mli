type t = Graphv_gles3.t

module Align : sig
  type t = int
  val no_flags : int
  val has : int -> flag:int -> bool
  val or_ : int -> int -> int
  val ( lor ) : int -> int -> int
  val remove : int -> flag:int -> int
  val left : t
  val center : t
  val right : t
  val top : t
  val middle : t
  val bottom : t
  val baseline : t

  val v_align : t -> t
  val h_align : t -> t
end

module BlendFactor : sig
  type t = Zero
       | One
       | Src_color
       | One_minus_src_color
       | Dst_color
       | One_minus_dst_color
       | Src_alpha
       | One_minus_src_alpha
       | Dst_alpha
       | One_minus_dst_alpha
       | Src_alpha_saturate
end

module Bounds : sig
  type t = {
    xmin : float;
    ymin : float;
    xmax : float;
    ymax : float;
  } 
  val scale : t -> float -> t
  val empty : t
end

module Buffer : sig
    module UByte : sig
        type t (*=
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t*)
      val create : int -> t
      val get : t -> int -> int
      val set : t -> int -> int -> unit
      val length : t -> int
      val sub : t -> int -> int -> t
      val empty : t 
    end
    module Float : sig
      type t
      val create : int -> t
      val get : t -> int -> float
      val set : t -> int -> float -> unit
      val length : t -> int
      val zero : t -> unit
      val blit : src:t -> s_off:int -> dst:t -> d_off:int -> len:int -> unit
    end
end

module Color : sig
  type t = {
    r : float;
    g : float;
    b : float;
    a : float;
  }
  val premultiply : t -> t
  val rgbaf : r:float -> g:float -> b:float -> a:float -> t
  val rgbf : r:float -> g:float -> b:float -> t
  val rgba : r:int -> g:int -> b:int -> a:int -> t
  val rgb : r:int -> g:int -> b:int -> t
  val white : t
  val black : t
  val transparent : t
  val lerp : t -> t -> a:float -> t
  val clamp : float -> float -> float -> float
  val transf : t -> float -> t
  val trans : t -> int -> t
  val hue : float -> float -> float -> float
  val hsl : h:float -> s:float -> l:float -> t
  val hsla : h:float -> s:float -> l:float -> a:int -> t
end

module CompositeOperation : sig
  type t =
    | Source_over
    | Source_in
    | Source_out
    | Atop
    | Destination_over
    | Destination_in
    | Destination_out
    | Destination_atop
    | Lighter
    | Copy
    | Xor

end

module CompositeOperationState : sig
  type t = {
    src_rgb : BlendFactor.t;
    dst_rgb : BlendFactor.t;
    src_alpha : BlendFactor.t;
    dst_alpha : BlendFactor.t;
  }
  val of_composite_operation : CompositeOperation.t -> t
end

module ImageFlags : sig
  type t
  val no_flags : t
  val generate_mipmaps : t
  val repeat_x : t
  val repeat_y : t
  val flip_y : t
  val premultiplied : t
  val nearest : t
  val ( lor ) : t -> t -> t
  val remove : t -> flag:t -> t
  val has : t -> flag:t -> bool
end

module LineCap : sig
  type t = Butt | Round | Square | Default
  val equal : t -> t -> bool
end

module LineJoin : sig
  type t = Miter | Bevel | Round
  val equal : t -> t -> bool
end

module Matrix : sig
  type t = {
    mutable m0 : float;
    mutable m1 : float;
    mutable m2 : float;
    mutable m3 : float;
    mutable m4 : float;
    mutable m5 : float;
  } 
  val create : unit -> t
  val copy : t -> t
  val zero : t -> unit
  val translate : t -> x:float -> y:float -> unit
  val get_average_scale : t -> float
  val multiply : dst:t -> src:t -> unit
  val transform_point : t -> float -> float -> float * float
  val premultiply : dst:t -> src:t -> unit
  val scale : t -> xs:float -> ys:float -> unit
  val inverse : dst:t -> src:t -> unit
  val rotate : t -> angle:float -> unit
  val identity : t -> unit
  val skew_x : t -> angle:float -> unit
  val skew_y : t -> angle:float -> unit
  val to_3x4 : t -> float array
  val is_flipped : t -> bool
  val print : t -> unit
end

module Winding : sig
  type t = CCW | CW
  val ccw : t
  val cw : t
  val hole : t
  val solid : t 
  val equal : t -> t -> bool
end

module Cache : sig
    type cached
    type kind = Stroke | Fill | Fill_stroke | Stroke_fill
    val begin_ : t -> unit
    val draw : t -> cached -> x:float -> y:float -> unit
    val save : t -> kind -> cached
end

val save : t -> unit
val restore : t -> unit
val reset : t -> unit
val set_device_pixel_ratio : t -> float -> unit
val set_shape_antialias : t -> enabled:bool -> unit
val set_miter_limit : t -> limit:float -> unit
val set_stroke_width : t -> width:float -> unit
val set_line_cap : t -> cap:LineCap.t -> unit
val set_line_join : t -> join:LineJoin.t -> unit

val begin_frame : t -> width:float -> height:float -> device_ratio:float -> unit
val cancel_frame : t -> unit
val end_frame : t -> unit

module Transform : sig
    val reset : t -> unit
    val transform : t -> Matrix.t -> unit
    val translate : t -> x:float -> y:float -> unit
    val rotate : t -> angle:float -> unit
    val skew_x : t -> angle:float -> unit
    val skew_y : t -> angle:float -> unit
    val scale : t -> x:float -> y:float -> unit
    val current_transform : t -> Matrix.t
    val deg_to_rad : float -> float
    val rad_to_deg : float -> float
end

module Scissor : sig
    val scissor : t -> x:float -> y:float -> w:float -> h:float -> unit
    val intersect : t -> x:float -> y:float -> w:float -> h:float -> unit
    val reset : t -> unit
end

module Global : sig
    val set_composite_operation : t -> op:CompositeOperationState.t -> unit

    val set_composite_blend_func : t -> src:BlendFactor.t -> dst:BlendFactor.t -> unit

    val set_composite_blend_func_separate :
        t
        -> src_rgb:BlendFactor.t
        -> dst_rgb:BlendFactor.t
        -> src_alpha:BlendFactor.t
        -> dst_alpha:BlendFactor.t
        -> unit

    val set_alpha : t -> alpha:float -> unit
end

val fill : t -> unit
val stroke : t -> unit

module Path : sig
    val begin_ : t -> unit
    val close : t -> unit
    val move_to : t -> x:float -> y:float -> unit
    val line_to : t -> x:float -> y:float -> unit
    val bezier_to : t -> c1x:float -> c1y:float -> c2x:float -> c2y:float -> x:float -> y:float -> unit
    val quad_to : t -> cx:float -> cy:float -> x:float -> y:float -> unit
    val rect : t -> x:float -> y:float -> w:float -> h:float -> unit
    val winding : t -> winding:Winding.t -> unit
    val arc : t -> cx:float -> cy:float -> r:float -> a0:float -> a1:float -> dir:Winding.t -> unit
    val arc_to : t -> x1:float -> y1:float -> x2:float -> y2:float -> radius:float -> unit
    val ellipse : t -> cx:float -> cy:float -> rx:float -> ry:float -> unit
    val circle : t -> cx:float -> cy:float -> r:float -> unit
    val rounded_rect : t -> x:float -> y:float -> w:float -> h:float -> r:float -> unit
    val rounded_rect_varying : 
        t -> x:float -> y:float -> w:float -> h:float
        -> top_left:float
        -> top_right:float
        -> bot_left:float
        -> bot_right:float
        -> unit
end

module Image : sig
    type image
    type data = Buffer.UByte.t
    val dummy : image
    val from_color : t -> data:Color.t array -> width:int -> height:int -> flags:ImageFlags.t -> image option
    val from_buffer : t -> data:data -> width:int -> height:int -> flags:ImageFlags.t -> image option
    val update_image : t -> image:image -> data:data -> bool
    val size : t -> image -> int * int
end

module Paint : sig
    type ctx = t
    type t

    val linear_gradient : 
        ctx -> sx:float -> sy:float -> ex:float -> ey:float -> icol:Color.t -> ocol:Color.t -> t

    val box_gradient :
        ctx -> x:float -> y:float -> w:float -> h:float -> r:float -> f:float -> icol:Color.t -> ocol:Color.t -> t

    val radial_gradient :
        ctx -> cx:float -> cy:float -> in_radius:float -> out_radius:float -> icol:Color.t -> ocol:Color.t -> t

    val image_pattern :
        ctx -> cx:float -> cy:float -> w:float -> h:float -> angle:float -> image:Image.image -> alpha:float -> t
end

val set_fill_color : t -> color:Color.t -> unit
val set_fill_paint : t -> paint:Paint.t -> unit

val set_stroke_color : t -> color:Color.t -> unit
val set_stroke_paint : t -> paint:Paint.t -> unit

module Text : sig 
    type font
    type bounds = {
        box : Bounds.t;
        advance : float;
    }
    val text : t -> x:float -> y:float -> ?start:int -> ?end_:int -> string -> unit
    val text_w : t -> x:float -> y:float -> ?start:int -> ?end_:int -> string -> float
    val find_font : t -> name:string -> font option
    val set_size : t -> size:float -> unit
    val set_blur : t -> blur:float -> unit
    val set_line_height : t -> height:float -> unit
    val set_letter_spacing : t -> spacing:float -> unit
    val set_align : t -> align:Align.t -> unit
    val set_font_face : t -> name:string -> unit
    val set_font_face_id : t -> id:int -> unit
    val create : t -> name:string -> file:string -> font option
    val bounds : t -> x:float -> y:float -> ?start:int -> ?end_:int -> string -> bounds
    type metrics = {
        ascender : float;
        descender : float;
        line_height : float;
    }
    val metrics : t -> metrics
    type text_row = {
        start_index : int;
        end_index : int;
        width : float;
        minx : float;
        maxx : float;
        next : int;
    }
    val break_lines : t -> break_width:float -> max_rows:int -> ?start:int -> ?end_:int -> lines:text_row array -> string -> int
    val make_empty_rows : int -> text_row array
    type glyph_position = {
        index : int;
        x : float;
        min_x : float;
        max_x : float;
    }
    val empty_glyph_position : glyph_position
    val glyph_positions :
      t ->
      x:float ->
      y:float -> ?start:int -> ?end_:int -> glyphs:glyph_position array -> string -> int
    val text_box : t -> x:float -> y:float -> break_width:float -> ?start:int -> ?end_:int -> string -> unit
    val box_bounds : t -> x:float -> y:float -> break_width:float -> ?start:int -> ?end_:int -> string -> Bounds.t

    val add_fallback_id : t -> font:font -> fallback:font -> unit
    val add_fallback : t -> name:string -> fallback:string -> unit
    val reset_fallback_id : t -> font:font -> unit
    val reset_fallback : t -> name:string -> unit
end
