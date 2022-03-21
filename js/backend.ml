module Gv = Graphv_webgl2
open Js_of_ocaml

module Window = struct
    type t = {
        ctx : WebGL.renderingContext Js.t;
        node : Dom_html.canvasElement Js.t; 
    }

    let set_title ~window ~title =
        Dom_html.document##.title := Js.string title

    let set_size ~window:_ ~width:_ ~height:_ =
        ()

    let get_size ~window =
        let w = Dom_html.window##.innerWidth in
        let h = Dom_html.window##.innerHeight in
        (w, h)

    let ctx_webgl canvas : WebGL.renderingContext Js.t =
        let open Js.Unsafe in
        let options = (Js.Unsafe.coerce (object%js
                val antialias = Js._false
                val stencil = Js._true
            end))
        in
        let ctx : WebGL.renderingContext Js.t =
          meth_call canvas "getContext" [|inject (Js.string "webgl2"); inject options|]
        in
        ctx
    ;;

    let scale_canvas (canvas : Dom_html.canvasElement Js.t) (width : int) (height : int) =
        let dpr = Dom_html.window##.devicePixelRatio in
        canvas##.style##.width := Printf.sprintf "%.2fpx" (float width /. dpr) |> Js.string;
        canvas##.style##.height := Printf.sprintf "%.2fpx" (float height /. dpr) |> Js.string;
        canvas##.width := width;
        canvas##.height := height;
    ;;

    let create ~width ~height ~title : t * Gv.t =
        let canvas = Dom_html.createCanvas Dom_html.document in
        scale_canvas canvas width height;
        Dom_html.document##.body##appendChild (canvas :> Dom.node Js.t) |> ignore;
        let ctx = ctx_webgl canvas in
        let open Gv.CreateFlags in
        let gv = Gv.create ~flags:(antialias lor stencil_strokes lor tesselate_afd) ctx in
        Gv.Text.create gv ~name:"mono" ~file:"courier new" |> ignore;
        Gv.Text.create gv ~name:"icons" ~file:"times new roman" |> ignore;
        let window = {
            ctx;
            node = canvas;
        } in
        window, gv
end

module Time = struct
    let now () : float =
        Js.Unsafe.pure_js_expr "performance.now()/1000."
end

module Run = struct

    type t = {
        mutable last_render_time : float;
    }

    let init () = 
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
            let rec draw _ =
                idle();
                let start = Time.now() in
                screen#drawAll;
                t.last_render_time <- Time.now() -. start;
                Dom_html.window##requestAnimationFrame (Js.wrap_callback draw) |> ignore;
            in
            Dom_html.window##requestAnimationFrame (Js.wrap_callback draw) |> ignore;
        | screens ->
                ()
                (*
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
            *)
    ;;
end

module Mouse = struct
    type button = int
    let button_left = 0
    let button_right = 2
end

module Cursor = struct
    type t = string

    let set_cursor ~(cursor : t) ~(window : Window.t) =
        (*GLFW.setCursor ~window ~cursor*)
        ()
end

module Clipboard = struct
    let get_string ~(window : Window.t) =
        ""

    let set_string ~(window : Window.t) ~string =
        ()
end

module Key = struct
    type action = Release | Press | Repeat
    type modifier = Shift | Control | Alt | Super
    type key =
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
        let open Window in
        window.node##.onmousedown := (Dom_html.handler (fun e ->
            t#mouseButtonCallbackEvent e##.button true [] |> ignore;
            Js._true
        ));
        window.node##.onmouseup := (Dom_html.handler (fun e ->
            t#mouseButtonCallbackEvent e##.button false [] |> ignore;
            Js._true
        ));
        window.node##.onmousemove := (Dom_html.handler (fun e ->
            let dpr = Dom_html.window##.devicePixelRatio in
            t#cursorPosCallbackEvent (float e##.offsetX *. dpr) (float e##.offsetY *. dpr) |> ignore;
            Js._true
        ));
        window.node##.onmouseover := (Dom_html.handler (fun _ ->
            t#cursorEnterCallbackEvent true;
            Js._true
        ));
        window.node##.onmouseout := (Dom_html.handler (fun _ ->
            t#cursorEnterCallbackEvent false;
            Js._true
        ));
        (*new ResizeObserver(outputsize).observe(textbox)*)
        ResizeObserver.observe ~node:window.node ~f:(fun a b ->
            t#resizeCallbackEvent (float window.node##.width) (float window.node##.height) |> ignore;
        ) () |> ignore;
        (*
        GLFW.setKeyCallback ~window:window ~f:(Some (fun _window key scancode action mods ->
            t#keyCallbackEvent key scancode action mods |> ignore
        )) |> ignore;

        GLFW.setCharCallback ~window:window ~f:(Some (fun _window codepoint ->
            t#charCallbackEvent codepoint |> ignore
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
        *)
end

module Context = struct
    open Window

    let get_framebuffer_size ~window =
        let w = window.ctx##.drawingBufferWidth in
        let h = window.ctx##.drawingBufferHeight in
        (w, h)

    let make_current ~window =
        ()

    let viewport ~window x y w h =
        window.ctx##viewport x y w h

    let clear ~window =
        let c = 0.17647058823529413 in
        let ctx = window.ctx in
        ctx##clearColor c c c 1.;
        ctx##clear (ctx##._COLOR_BUFFER_BIT_ lor ctx##._DEPTH_BUFFER_BIT_ lor ctx##._STENCIL_BUFFER_BIT_);
end
