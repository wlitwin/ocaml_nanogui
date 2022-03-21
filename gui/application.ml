type resources = {
    mono_font : string;
    icon_font : string;
}

type t = {
    resources : resources;
    app : Backend.Run.t;
}

let init resources =
    let app = Backend.Run.init() in
    {
        resources;
        app
    }
;;

let create_screen ?(swap_interval=1) ~title ~width ~height _t =
    let window, gv = Backend.Window.create ~width ~height ~title in
    let open Screen in
    let screen = new screen Vec2.(mk (float width) (float height)) gv window in
    Backend.Listener.listen (screen :> Backend.Listener.t) window;
    screen

let run ?(idle=fun () -> ()) t (screens : Screen.screen list) =
    Backend.Run.run ~idle t.app (screens :> Backend.Run.screen list)
