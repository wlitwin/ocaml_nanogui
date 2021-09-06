[@@@landmark "auto"]

open Widget
open Tgles2

class screen initial_size nvg_context glfw_window = object(self)
    inherit widget None as super

    val mutable caption : string = "GUI"
    val mutable background : color = Gv.Color.(rgb ~r:64 ~g:64 ~b:64)
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
            let ww = float ww in
            let wh = float wh in
            let ratio = (float fbw /. ww) in
            pixelRatio <- ratio;
            Gl.viewport 0 0 fbw fbh;
            Gv.begin_frame nvgContext ~width:ww ~height:wh ~device_ratio:pixelRatio;

            (*
            Gv.(
                let ctx = nvgContext in
            begin_path ctx;
            rect ctx 0. 0. size.a size.b;
            fill_color ctx theme#windowFillFocused;
            fill ctx;
            );
            *)

            Gv.Transform.translate nvgContext ~x:position.a ~y:position.b;
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
                let open Gv in
                let nvg = nvgContext in
                Text.set_font_face nvg ~name:"mono";
                Text.set_size nvg ~size:15.;
                Text.set_align nvg ~align:Align.(left lor top);
                Text.set_line_height nvg ~height:1.1;
                let open Float in
                let pos = Vec2.(widget#absolutePosition + mk (widget#width*.0.5) (widget#height+.10.)) in
                let b = (Text.bounds nvg ~x:pos.a ~y:pos.b tooltip).box in
                let b, h_off =
                    let h_off = (b.xmax - b.xmin)*0.5 in
                    if Float.(h_off > tooltip_width*0.5) then (
                        Text.set_align nvg ~align:Align.(left lor top);
                        let b = Text.box_bounds nvg ~x:pos.a ~y:pos.b ~break_width:tooltip_width tooltip in
                        b, (b.xmax - b.xmin)*0.5
                    ) else b, h_off
                in
                Global.set_alpha nvg ~alpha:Float.((min 1. (2.*(elapsed-0.4)))*0.8);
                Path.begin_ nvg;
                set_fill_color nvg ~color:Color.black;
                Path.rounded_rect nvg 
                    ~x:(b.xmin - 4. - h_off)
                    ~y:(b.ymin - 4.)
                    ~w:(b.xmax - b.xmin + 8.)
                    ~h:(b.ymax - b.ymin + 8.)
                    ~r:3.;
                let px = pos.a in
                Path.move_to nvg ~x:px ~y:(b.ymin - 10.);
                Path.line_to nvg ~x:(px + 7.) ~y:(b.ymin + 1.);
                Path.line_to nvg ~x:(px - 7.) ~y:(b.ymin + 1.);
                fill nvg;

                set_fill_color nvg ~color:Color.white;
                Text.set_blur nvg ~blur:0.;
                Text.text_box nvg ~x:(pos.a-h_off) ~y:pos.b ~break_width:tooltip_width tooltip;
            ) |> ignore;

            Gv.end_frame nvgContext;
        )
end

