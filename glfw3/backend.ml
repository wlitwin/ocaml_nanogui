module Gv = Graphv_gles3

module Window = struct
    type t = GLFW.window

    let set_title ~window ~title =
        GLFW.setWindowTitle ~window ~title

    let set_size ~window ~width ~height =
        GLFW.setWindowSize ~window ~width ~height

    let get_size ~window =
        GLFW.getWindowSize ~window

    let create ~width ~height ~title : t * Gv.t =
        let window = GLFW.createWindow ~width ~height ~title () in
        let open Gv.CreateFlags in
        GLFW.makeContextCurrent ~window:(Some window);
        GLFW.swapInterval ~interval:1;
        let gv = Gv.create ~flags:(antialias lor stencil_strokes lor tesselate_afd) () in
        Gv.Text.create gv ~name:"mono" ~file:"./assets/UbuntuMono-Regular.ttf" |> ignore;
        Gv.Text.create gv ~name:"icons" ~file:"./assets/entypo.ttf" |> ignore;
        window, gv
end

module Run = struct

    module GLFWExtras = struct
      open Ctypes
      open Foreign

      let glfwSetErrorCallback : (int -> string -> unit) -> (int -> string -> unit) =
          let errorfun = int @-> string @-> returning void in
          foreign "glfwSetErrorCallback" (funptr errorfun @-> returning (funptr errorfun))
      ;;
    end

    type t = {
        mutable last_render_time : float;
    }

    let init () = 
        let errorcb error desc =
          Printf.printf "GLFW error %d: %s\n%!" error desc
        in
        GLFW.init();
        at_exit GLFW.terminate;
        let _res = GLFWExtras.glfwSetErrorCallback errorcb in
        GLFW.windowHint ~hint:GLFW.ClientApi ~value:GLFW.OpenGLESApi;
        GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:3;
        GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:2;
        { last_render_time = 0.; }
    ;;

    type screen = <
        performLayoutEntry : unit;
        window : Window.t; 
        drawAll : unit;
    >

    let run ?(idle=fun () -> ()) (t : t) (screens : screen list) =
        (* One pass, do initial layout *)
        List.iter (fun sc ->
            sc#performLayoutEntry;
        ) screens;
        match screens with
        | [] -> ()
        | [screen] ->
            let window = screen#window in
            while not GLFW.(windowShouldClose ~window) do
                GLFW.pollEvents();
                idle();
                let start = GLFW.getTime() in
                screen#drawAll;
                t.last_render_time <- GLFW.(getTime()) -. start;
                GLFW.swapBuffers ~window
            done
        | screens ->
            let draw_screen (sc : screen) =
                let window = sc#window in
                if GLFW.windowShouldClose ~window
                then false
                else (
                    GLFW.makeContextCurrent ~window:(Some window);
                    sc#drawAll;
                    GLFW.swapBuffers ~window;
                    true
                )
            in
            let render_screens (lst : screen list) =
                GLFW.pollEvents();
                idle();
                let lst = List.filter draw_screen lst in
                (*
                List.iter (fun sc ->
                    let window = sc#glfwWindow in
                    GLFW.makeContextCurrent ~window:(Some window);
                ) lst;
                *)
                lst
            in
            let rec loop = function
                | [] -> ()
                | lst -> loop (render_screens lst)
            in
            loop screens
    ;;
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

module Listener = struct
    type t = <
        cursorPosCallbackEvent : float -> float -> bool;
        cursorEnterCallbackEvent : bool -> unit;
        keyCallbackEvent : Key.key -> int -> Key.action -> Key.modifier list -> unit;
        charCallbackEvent : int -> unit;
        mouseButtonCallbackEvent : Mouse.button -> bool -> Key.modifier list -> unit;
        scrollCallbackEvent : float -> float -> unit;
        resizeCallbackEvent : float -> float -> bool;
        dropCallbackEvent : string list -> unit;
    >

    let listen (t : t) (window : Window.t) =
        GLFW.setCursorPosCallback ~window:window ~f:(Some (fun _window x y ->
            t#cursorPosCallbackEvent x y |> ignore
        )) |> ignore;

        GLFW.setCursorEnterCallback ~window ~f:(Some (fun _window entered ->
            t#cursorEnterCallbackEvent entered
        )) |> ignore;

        GLFW.setKeyCallback ~window:window ~f:(Some (fun _window key scancode action mods ->
            t#keyCallbackEvent key scancode action mods |> ignore
        )) |> ignore;

        GLFW.setCharCallback ~window:window ~f:(Some (fun _window codepoint ->
            t#charCallbackEvent codepoint |> ignore
        )) |> ignore;

        GLFW.setMouseButtonCallback ~window:window ~f:(Some (fun _window button action mods ->
            (*Printf.printf "GLFW Mouse Button %d %b\n%!" button action;*)
            t#mouseButtonCallbackEvent button action mods |> ignore
        )) |> ignore;

        GLFW.setScrollCallback ~window:window ~f:(Some (fun _window x y ->
            t#scrollCallbackEvent x y |> ignore
        )) |> ignore;

        GLFW.setFramebufferSizeCallback ~window:window ~f:(Some (fun _window w h ->
            t#resizeCallbackEvent (Float.of_int w) (Float.of_int h) |> ignore
        )) |> ignore;

        GLFW.setDropCallback ~window:window ~f:(Some (fun _window files ->
            t#dropCallbackEvent files |> ignore
        )) |> ignore;
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
