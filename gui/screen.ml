[@@@landmark "auto"]

open Widget
open Tgles2

module NV = Nanovg

class screen initial_size nvg_context glfw_window = object(self)
    inherit widget None as super

    val mutable caption : string = "GUI"
    val mutable background : color = Nanovg.(rgb 64 64 64)
    val mutable resizeCallback : Vec2.t -> unit = fun _ -> ()
    val mutable mousePos : Vec2.t = Vec2.zero
    val mutable nvgContext : graphics_context = nvg_context
    val mutable pixelRatio : float = 1.
    val mutable mouseState : int = 0
    val mutable modifiers : GLFW.key_mod list = []
    val mutable fbSize : Vec2.t = Vec2.zero
    val mutable dragActive : bool = false
    val mutable dragWidget : widget option = None
    val mutable lastInteraction : float = 0.
    val mutable processEvents : bool = true
    val mutable focusPath : widget V.vector = V.create()
    val mutable popups : widget list = []
    val mutable lastLayoutTime : float = 0.
    val mutable layoutDirty : bool = true
    val window : GLFW.window = glfw_window

    initializer
        size <- initial_size;
        lastInteraction <- GLFW.getTime();

    method lastLayoutTime = lastLayoutTime

    method glfwWindow = window

    method nvgContext = nvgContext

    method caption = caption
    method setCaption c = 
        caption <- c;
        GLFW.setWindowTitle ~window ~title:caption

    method! setSize size =
        super#setSize size;
        GLFW.setWindowSize ~window ~width:(size.a |> Int.of_float) ~height:(size.b |> Int.of_float)

    method background = background
    method setBackground b = background <- b

    method drawAll =
        if layoutDirty then (
            layoutDirty <- false;
            self#performLayoutEntry;
        );

        let c = 0.17647058823529413 in
        Gl.clear_color c c c 1.;
        Gl.clear Gl.(color_buffer_bit + depth_buffer_bit + stencil_buffer_bit);
        self#drawContents;
        self#drawWidgets;

    method drawContents = ()

    method pixelRatio = 1.

    method dropEvent (_files : string list) : bool = false

    method resizeEvent (size : Vec2.t) =
        resizeCallback size;
        (*self#performLayoutEntry;*)
        self#setLayoutDirty;
        false

    method resizeCallback = resizeCallback
    method setResizeCallback cb = resizeCallback <- cb

    method mousePos = mousePos

    method performLayoutEntry =
        let start = GLFW.getTime() in
        super#performLayout nvgContext;
        lastLayoutTime <- ((GLFW.getTime()) -. start) *. 1000.;

    method cursorPosCallbackEvent (x : float) (y : float) =
        lastInteraction <- GLFW.getTime();
        let p = Vec2.mk 
            ((x /. pixelRatio) -.1.) ((y /. pixelRatio)-.2.) 
        (* TODO why subtract? *) in
    
        let open Option.Let_syntax in
        let event = MouseDrag {pos=p; rel=Vec2.(p - mousePos); button=mouseState; mods=modifiers} in
        if not dragActive then (
            (*
            (find_widget p >>= fun widget ->
            widget#cursor >>| fun cursor ->
            GLFW.setCursor ~window ~cursor
            ) |> ignore
            *)
            ()
        ) else (
            (dragWidget >>| fun widget ->
            dispatch_event event widget;
            ) |> ignore
        );

        update_mouse_focus self p Vec2.(p - mousePos);

        (* TODO - make sure default prevented? *)
        (*
        if not event.default_prevented then (
            ret := self#mouseMotionEvent ~pos:p ~rel:Vec2.(p - mousePos) mouseState modifiers
        );
        *)
        let motion = MouseMotion {pos=p; rel=Vec2.(p - mousePos); button=mouseState; mods=modifiers} in
        dispatch_event_by_position (self :> widget) motion p;

        mousePos <- p;
        true

    method setLayoutDirty =
        layoutDirty <- true

    method! asScreen : screen_spec option =
        Some (self :> screen_spec)

    method cursorEnterCallbackEvent entered =
        if entered = false then (
            let event = MouseEnter {pos=Vec2.(mk1 ~-.1.); entered=false} in
            dispatch_event_by_position (self :> widget) event mousePos
        )

    method mouseButtonCallbackEvent (button : int) (down : bool) (mods : GLFW.key_mod list) = 
        modifiers <- mods;
        lastInteraction <- GLFW.getTime();

        let same_widget a b =
            match a, b with
            | None, None -> false
            | Some _, None -> false
            | None, Some _ -> false
            | Some a, Some b -> phys_equal a b
        in

        if down then (
            mouseState <- mouseState lor (1 lsl button);
        ) else (
            mouseState <- mouseState land (lnot (1 lsl button));
        );
        
        let open Option.Let_syntax in
        let drop_widget, path = find_widget (self :> widget) mousePos in
        if dragActive && Bool.(down = false) && not (same_widget drop_widget dragWidget) then (
            (dragWidget >>| fun d ->
            dispatch_event (MouseButton {pos=mousePos; button; down=false; mods=modifiers}) d;
            ) |> ignore
        );

        (drop_widget >>= fun dropWidget ->
         dropWidget#cursor >>| fun cursor ->
         GLFW.setCursor ~window ~cursor
        ) |> ignore;

        if down && (button = GLFW.mouse_button_left || button = GLFW.mouse_button_right) then (
            dragWidget <- drop_widget;
            if same_widget (Some (self :> widget)) dragWidget then (
                dragWidget <- None;
            );
            dragActive <- Option.is_some dragWidget;
            if not dragActive then (
                self#updateFocus None
            )
        ) else (
            dragActive <- false;
            dragWidget <- None;
        );

        let event = MouseButton {pos=mousePos; button; down; mods=modifiers} in
        match drop_widget with
        | None -> dispatch_event event (self :> widget)
        | Some dw -> dispatch_event_with_chain event dw path

    method addPopup (w : widget_spec) : unit =
        popups <- w :: popups

    method removePopup (w : widget_spec) : unit =
        popups <- List.filter popups ~f:(fun p -> phys_equal w p |> not)

    method keyCallbackEvent (key : GLFW.key) (scancode : int) (action : GLFW.key_action) (mods : GLFW.key_mod list) = 
        lastInteraction <- GLFW.getTime();
        self#dispatchEventToFocused (KeyboardKey {key; scancode; action; mods})

    method charCallbackEvent (codepoint : int) = 
        lastInteraction <- GLFW.getTime();
        self#dispatchEventToFocused (KeyboardChar codepoint)

    method dropCallbackEvent (filenames : string list) =
        dispatch_event_by_position (self :> widget) (FileDrop filenames) mousePos;

    method scrollCallbackEvent (x : float) (y : float) = 
        lastInteraction <- GLFW.getTime();
        let event = ScrollEvent {pos=mousePos; rel=Vec2.(mk x y)} in
        let self = (self :> widget) in
        let before = find_widget self mousePos in
        dispatch_event_by_position self event mousePos;
        let after = find_widget self mousePos in
        update_focus_paths before after mousePos

    method private dispatchEventToFocused event =
        if not (V.is_empty focusPath) then (
            let first = V.get focusPath 0 in
            dispatch_event event first
        )

    method resizeCallbackEvent (_width : float) (_height : float) = 
        lastInteraction <- GLFW.getTime();
        let fbw, fbh = GLFW.getFramebufferSize ~window in
        let ww, wh = GLFW.getWindowSize ~window in

        fbSize <- Vec2.mk Float.(of_int fbw) Float.(of_int fbh);
        size <- Vec2.mk Float.(of_int ww) Float.(of_int wh);

        self#resizeEvent size

    method updateFocus (widget : widget_spec option) : unit =
        self#dispatchEventToFocused (Focus false);
        V.clear focusPath;
        let rec loop = function
            | None -> ()
            | Some w ->
                V.push focusPath w;
                loop w#parent
        in
        loop widget;
        self#dispatchEventToFocused (Focus true);

    method drawWidgets =
        if visible then (
            GLFW.makeContextCurrent ~window:(Some window);

            let fbw, fbh = GLFW.getFramebufferSize ~window in
            let ww, wh = GLFW.getWindowSize ~window in
            let ratio = Float.(of_int fbw / of_int ww) in
            pixelRatio <- ratio;
            Gl.viewport 0 0 fbw fbh;
            Nanovg.begin_frame nvgContext ww wh pixelRatio;

            (*
            NV.(
                let ctx = nvgContext in
            begin_path ctx;
            rect ctx 0. 0. size.a size.b;
            fill_color ctx theme#windowFillFocused;
            fill ctx;
            );
            *)

            Nanovg.translate nvgContext position.a position.b;
            self#draw nvgContext;

            (* TODO - allow tooltip to be arbitrary widget *)
            let elapsed = GLFW.getTime() -. lastInteraction in
            if (elapsed > 0.5) then (
                (* Draw tooltips *)
                let widget, _ = find_widget (self :> widget) mousePos in
                let open Option in
                widget >>= fun widget ->
                widget#tooltip >>| fun tooltip ->
                let tooltip_width = 150. in
                let bounds = Ctypes.(allocate_n float ~count:4) in
                let open Nanovg in
                let nvg = nvgContext in
                font_face nvg "mono";
                font_size nvg 15.;
                text_align nvg Align.(left lor top);
                text_line_height nvg 1.1;
                let open Float in
                let pos = Vec2.(widget#absolutePosition + mk (widget#width*.0.5) (widget#height+.10.)) in
                text_bounds nvg pos.a pos.b tooltip null_char bounds |> ignore;
                let get_bounds n = Ctypes.(!@(bounds +@ n)) in
                let h = 
                    let h = (get_bounds 2 - get_bounds 0)*0.5 in
                    if Float.(h > tooltip_width*0.5) then (
                        text_align nvg Align.(left lor top);
                        text_box_bounds nvg pos.a pos.b tooltip_width tooltip null_char bounds;
                        (get_bounds 2 - get_bounds 0)*0.5
                    ) else h
                in
                global_alpha nvg Float.((min 1. (2.*(elapsed-0.4)))*0.8);
                begin_path nvg;
                fill_color nvg (rgba 0 0 0 255);
                rounded_rect nvg (get_bounds 0 - 4. - h)
                                 (get_bounds 1 - 4.)
                                 (get_bounds 2 - get_bounds 0 + 8.)
                                 (get_bounds 3 - get_bounds 1 + 8.)
                                 3.;
                let px = pos.a in
                move_to nvg px (get_bounds 1 - 10.);
                line_to nvg (px + 7.) (get_bounds 1 + 1.);
                line_to nvg (px - 7.) (get_bounds 1 + 1.);
                fill nvg;

                fill_color nvg (rgb 255 255 255);
                font_blur nvg 0.;
                text_box nvg (pos.a-h) pos.b tooltip_width tooltip null_char;
            ) |> ignore;

            Nanovg.end_frame nvgContext;
        )
end

