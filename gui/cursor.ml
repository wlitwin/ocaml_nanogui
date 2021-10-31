type t = GLFW.cursor
type window = GLFW.window

let set_cursor (cursor : t) (window : window) =
    GLFW.setCursor ~window ~cursor


