type resources = {
    mono_font : string;
    icon_font : string;
}

type t = {
    resources : resources;
}

module GLFWExtras = struct
  open Ctypes
  open Foreign

  let glfwSetErrorCallback : (int -> string -> unit) -> (int -> string -> unit) =
      let errorfun = int @-> string @-> returning void in
      foreign "glfwSetErrorCallback" (funptr errorfun @-> returning (funptr errorfun))
  ;;
end

let init resources =
    let errorcb error desc =
      Printf.printf "GLFW error %d: %s\n%!" error desc
    in

    GLFW.init();
    at_exit GLFW.terminate;
    let _res = GLFWExtras.glfwSetErrorCallback errorcb in
    GLFW.windowHint ~hint:GLFW.ClientApi ~value:GLFW.OpenGLESApi;
    GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:2;
    GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:0;
    {resources}
;;

(* Don't let the OCaml GC free the font memory arrays *)
let mono_keep_alive_ref = ref None
let icon_keep_alive_ref = ref None

exception Failed_to_load_font of string
let create_screen ?(swap_interval=1) ~title ~width ~height {resources=_} =
    (* TODO - is sharing the context faster? *)
    let window = GLFW.createWindow ~width ~height ~title () in
    (* Make the window's context current *)
    GLFW.makeContextCurrent ~window:(Some window);
    GLFW.swapInterval ~interval:swap_interval;

    (* Create nanoVG *)
    let nvg =
        let open Nanovg.Create_flags in
        try
            Nanovg.create_gles2 (antialias lor stencil_strokes lor debug)
        with Nanovg.Memory_error ->
            Caml.print_endline "Could not initialize nanoVG\n";
            Caml.exit ~-1;
    in

    (*
    let add_font name path =
        let font = Nanovg.create_font nvg name path in
        if font < 0 then (
            raise (Failed_to_load_font name)
        );
    in
    add_font "mono" resources.mono_font;
    add_font "icons" resources.icon_font;
    *)

    let add_font_mem name blob keep_alive_ref =
        let blob_array = Ctypes.CArray.of_string blob in
        keep_alive_ref := Some blob_array;
        let start_ptr = Ctypes.CArray.start blob_array in
        let font = Nanovg.create_font_mem nvg name Ctypes.(start_ptr |> to_voidp |> from_voidp uchar) Ctypes.CArray.(length blob_array) 1 in
        if font < 0 then (
            raise (Failed_to_load_font name)
        )
    in
    add_font_mem "mono" Font_blobs.ubuntu_mono mono_keep_alive_ref;
    add_font_mem "icons" Font_blobs.entypo icon_keep_alive_ref;

    let open Screen in
    let screen = new screen Vec2.(mk (float width) (float height)) nvg window in
    GLFW.setCursorPosCallback ~window:window ~f:(Some (fun _window x y ->
        screen#cursorPosCallbackEvent x y |> ignore
    )) |> ignore;

    GLFW.setCursorEnterCallback ~window ~f:(Some (fun _window entered ->
        screen#cursorEnterCallbackEvent entered
    )) |> ignore;

    GLFW.setKeyCallback ~window:window ~f:(Some (fun _window key scancode action mods ->
        screen#keyCallbackEvent key scancode action mods |> ignore
    )) |> ignore;

    GLFW.setCharCallback ~window:window ~f:(Some (fun _window codepoint ->
        screen#charCallbackEvent codepoint |> ignore
    )) |> ignore;

    GLFW.setMouseButtonCallback ~window:window ~f:(Some (fun _window button action mods ->
        (*Printf.printf "GLFW Mouse Button %d %b\n%!" button action;*)
        screen#mouseButtonCallbackEvent button action mods |> ignore
    )) |> ignore;

    GLFW.setScrollCallback ~window:window ~f:(Some (fun _window x y ->
        screen#scrollCallbackEvent x y |> ignore
    )) |> ignore;

    GLFW.setFramebufferSizeCallback ~window:window ~f:(Some (fun _window w h ->
        screen#resizeCallbackEvent (Float.of_int w) (Float.of_int h) |> ignore
    )) |> ignore;

    GLFW.setDropCallback ~window:window ~f:(Some (fun _window files ->
        screen#dropCallbackEvent files |> ignore
    )) |> ignore;

    screen

let last_render_time = ref 0.

let run ?(idle=fun () -> ()) screens =
    (* One pass, do initial layout *)
    List.iter (fun sc ->
        sc#performLayoutEntry;
    ) screens;
    match screens with
    | [] -> ()
    | [screen] ->
        let window = screen#glfwWindow in
        while not GLFW.(windowShouldClose ~window) do
            GLFW.pollEvents();
            idle();
            let start = GLFW.getTime() in
            screen#drawAll;
            last_render_time := GLFW.(getTime()) -. start;
            GLFW.swapBuffers ~window
        done
    | screens ->
        let draw_screen (sc : Screen.screen) =
            let window = sc#glfwWindow in
            if GLFW.windowShouldClose ~window
            then false
            else (
                GLFW.makeContextCurrent ~window:(Some window);
                sc#drawAll;
                GLFW.swapBuffers ~window;
                true
            )
        in
        let render_screens lst =
            GLFW.pollEvents();
            idle();
            let lst = List.filter lst ~f:draw_screen in
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
