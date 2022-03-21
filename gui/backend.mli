module Window : sig
  type t = GLFW.window
  val set_title : window:t -> title:string -> unit
  val set_size : window:t -> width:int -> height:int -> unit
  val get_size : window:t -> int * int
end
module Time : sig
    val now : unit -> float
end
module Mouse : sig
    type button = int
    val button_left : button
    val button_right : button
end
module Cursor : sig
    type t = GLFW.cursor
    val set_cursor : cursor:t -> window:Window.t -> unit
end
module Clipboard : sig
    val get_string : window:Window.t -> string
    val set_string : window:Window.t -> string:string -> unit
end
module Key : sig
  type action = GLFW.key_action = Release | Press | Repeat 
  type modifier  = GLFW.key_mod = Shift | Control | Alt | Super
  type key = GLFW.key =
    | Unknown
    | Space
    | Apostrophe
    | Comma
    | Minus
    | Period
    | Slash
    | Num0
    | Num1
    | Num2
    | Num3
    | Num4
    | Num5
    | Num6
    | Num7
    | Num8
    | Num9
    | Semicolon
    | Equal
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z
    | LeftBracket
    | Backslash
    | RightBracket
    | GraveAccent
    | World1
    | World2
    | Escape
    | Enter
    | Tab
    | Backspace
    | Insert
    | Delete
    | Right
    | Left
    | Down
    | Up
    | PageUp
    | PageDown
    | Home
    | End
    | CapsLock
    | ScrollLock
    | NumLock
    | PrintScreen
    | Pause
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12
    | F13
    | F14
    | F15
    | F16
    | F17
    | F18
    | F19
    | F20
    | F21
    | F22
    | F23
    | F24
    | F25
    | Kp0
    | Kp1
    | Kp2
    | Kp3
    | Kp4
    | Kp5
    | Kp6
    | Kp7
    | Kp8
    | Kp9
    | KpDecimal
    | KpDivide
    | KpMultiply
    | KpSubtract
    | KpAdd
    | KpEnter
    | KpEqual
    | LeftShift
    | LeftControl
    | LeftAlt
    | LeftSuper
    | RightShift
    | RightControl
    | RightAlt
    | RightSuper
    | Menu
end
module Listener : sig
    type t = <
        cursorPosCallback : int -> int -> bool;
        cursorEnterCallback : bool -> unit;
        keyCallbackEvent : Key.key -> int -> Key.action -> Key.modifier list -> unit;
        charCallbackEvent : int -> unit;
        mouseButtonCallbackEvent : Mouse.button -> bool -> Key.modifier list -> unit;
        scrollCallbackEvent : int -> int -> unit;
        resizeCallbackEvent : float -> float -> bool;
        dropCallbackEvent : string list -> unit;
    >
end
module Context : sig
    val get_framebuffer_size : window:Window.t -> int * int
    val make_current : window:Window.t option -> unit
    val clear : window:Window.t -> unit
    val viewport : window:Window.t -> int -> int -> int -> int -> unit
end
