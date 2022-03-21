module Window = struct
    type t = GLFW.window

    let set_title ~window ~title =
        GLFW.setWindowTitle ~window ~title

    let set_size ~window ~width ~height =
        GLFW.setWindowSize ~window ~width ~height

    let get_size ~window =
        GLFW.getWindowSize ~window
end

module Time = struct
    let now () =
        GLFW.getTime()
end

module Mouse = struct
    type button = int
    let button_left = GLFW.mouse_button_left
    let button_right = GLFW.mouse_button_right
end

module Cursor = struct
    type t = GLFW.cursor

    let set_cursor ~(cursor : t) ~(window : Window.t) =
        GLFW.setCursor ~window ~cursor
end

module Clipboard = struct
    let get_string ~window =
        GLFW.getClipboardString ~window

    let set_string ~window ~string =
        GLFW.setClipboardString ~window ~string
end

module Key = struct
    type action = GLFW.key_action = Release | Press | Repeat
    type modifier = GLFW.key_mod = Shift | Control | Alt | Super
    type key = GLFW.key =
      Unknown
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

module Context = struct
    open Tgles2 

    let get_framebuffer_size ~window =
        GLFW.getFramebufferSize ~window

    let make_current ~window =
        GLFW.makeContextCurrent ~window

    let viewport ~window:_ x y w h =
        Gl.viewport x y w h

    let clear ~window:_ =
        let c = 0.17647058823529413 in
        Gl.clear_color c c c 1.;
        Gl.clear Gl.(color_buffer_bit + depth_buffer_bit + stencil_buffer_bit);
end
