type t

type resources = {
    mono_font : string;
    icon_font : string;
}

val last_render_time : float ref
val init : resources -> t
val create_screen : ?swap_interval:int -> title:string -> width:int -> height:int -> t -> Screen.screen
val run : ?idle:(unit -> unit) -> t -> Screen.screen list -> unit
