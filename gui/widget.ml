module Gv = Graphv_gles2_native

module Event : sig
    type phase = Down
               | Up

    type ('a, 'b) event = {
        value : 'a;
        target : 'b;
        phase : phase;
        (* Mutable stop_immediate, prevent sibiling handlers + parents *)
        mutable stop_immediate : bool;
        mutable default_prevented : bool;
        mutable stop_propagation : bool;
    }

    val create : 'a -> 'b -> phase -> ('a, 'b) event
    val stop : ('a, 'b) event -> unit
end = struct
    type phase = Down
               | Up

    type ('a, 'b) event = {
        value : 'a;
        target : 'b;
        phase : phase;
        mutable stop_immediate : bool;
        mutable default_prevented : bool;
        mutable stop_propagation : bool;
    }

    let stop e = e.stop_propagation <- true

    let create value target phase = {
        value;
        target;
        phase;
        stop_immediate = false;
        default_prevented = false;
        stop_propagation = false;
    }
end

module Handlers : sig
    open Event
    type ('a, 'b) t
    type ('a, 'b) handler = ('a, 'b) event -> unit

    type id
    val create : unit -> ('a, 'b) t
    val add_handler : ('a, 'b) t -> ('a, 'b) handler -> phase -> id
    val remove_handler : ('a, 'b) t -> id -> unit
    val ( +=  ) : ('a, 'b) t -> ('a, 'b) handler -> id
    val ( +=! ) : ('a, 'b) t -> ('a, 'b) handler -> id
    val ( -= )  : ('a, 'b) t -> id -> unit
    val invoke : ('a, 'b) t -> ('a, 'b) event -> unit
end = struct
    open Event
    type ('a, 'b) handler = ('a, 'b) event -> unit
    type ('a, 'b) t = {
        mutable handlers : (int * phase * ('a, 'b) handler) list;
        mutable next_id : int;
    }

    type id = int

    let create () = {
        handlers = [];
        next_id = -1;
    }

    let get_next_id t =
        t.next_id <- t.next_id + 1;
        t.next_id

    let add_handler t fn phase =
        let id = get_next_id t in
        t.handlers <- (id, phase, fn) :: t.handlers;
        id
    ;;

    let remove_handler t id =
        t.handlers <- List.filter t.handlers ~f:(fun (fn_id, _, _) ->
            fn_id <> id
        );
    ;;

    let ( += )  t fn = add_handler t fn Up
    let ( +=! ) t fn = add_handler t fn Down
    let ( -= )  t id = remove_handler t id

    let invoke t event =
        let rec loop = function
            | [] -> ()
            | (_, phase, fn) :: tl ->
                if phase = event.phase then (
                    fn event;
                    if event.stop_immediate
                    then ()
                    else loop tl
                ) else loop tl
        in
        loop t.handlers       
end

type modifiers = GLFW.key_mod list
type ui_event = ..
type ui_event += MouseButton of { pos:Vec2.t; button:int; down:bool; mods:modifiers }
               | MouseMotion of { pos:Vec2.t; rel:Vec2.t; button:int; mods:modifiers }
               | MouseEnter of { pos:Vec2.t; entered:bool }
               | MouseDrag of { pos:Vec2.t; rel:Vec2.t; button:int; mods:modifiers }
               | ScrollEvent of {pos:Vec2.t; rel:Vec2.t}
               | Focus of bool
               | KeyboardKey of { key:GLFW.key; scancode:int; action:GLFW.key_action; mods:modifiers }
               | KeyboardChar of int
               | FileDrop of string list (* filenames *)
let split_bounds bounds : float * float * float * float =
    let open Ctypes in
    let minx = !@bounds in
    let miny = !@(bounds +@ 1) in
    let maxx = !@(bounds +@ 2) in
    let maxy = !@(bounds +@ 3) in
    minx, miny, maxx, maxy
;;

type graphics_context = Gv.t
type color = Gv.Color.t

type layoutable = <
    fixedSize : Vec2.t option;
    preferredSize : graphics_context -> Vec2.t;
    setSize : Vec2.t -> unit;
    setPosition : Vec2.t -> unit;
    performLayout : graphics_context -> unit;
>

class virtual togglableImpl = object
    val virtual mutable selected : bool
    val virtual mutable changeCallback : bool -> unit

    method changeCallback = changeCallback
    method setChangeCallback c = changeCallback <- c
    method isSet = selected
    method set s = selected <- s;
end

type togglable = <
    setChangeCallback : (bool -> unit) -> unit;
    changeCallback : (bool -> unit);
    isSet : bool;
    set : bool -> unit;
>

type alignment = Left | Center | Right

let to_utf8 (c : int) =
    let n = 
        if c < 0x80 then 1
        else if c < 0x800 then 2
        else if c < 0x10000 then 3
        else if c < 0x200000 then 4
        else if c < 0x4000000 then 5
        else if c <= 0x7fffffff then 6
        else 6
    in
    let seq = Bytes.create n in
    let ( .%()<- ) b i v = Bytes.set b i (char_of_int v) in
    let rec loop n c =
        match n with
        | 6 -> seq.%(5) <- 0x80 lor (c land 0x3f); loop (n-1) ((c lsr 6) lor 0x4000000)
        | 5 -> seq.%(4) <- 0x80 lor (c land 0x3f); loop (n-1) ((c lsr 6) lor 0x200000)
        | 4 -> seq.%(3) <- 0x80 lor (c land 0x3f); loop (n-1) ((c lsr 6) lor 0x10000)
        | 3 -> seq.%(2) <- 0x80 lor (c land 0x3f); loop (n-1) ((c lsr 6) lor 0x800)
        | 2 -> seq.%(1) <- 0x80 lor (c land 0x3f); loop (n-1) ((c lsr 6) lor 0xc0)
        | 1 -> seq.%(0) <- c
        | _ -> ()
    in
    loop n c;
    Bytes.to_string seq
;;

let phys_equal = ( == )

let rgba r g b a = 
    Gv.Color.rgba ~r ~g ~b ~a

class theme = 
object(self)
    method standardFontSize = 16.
    method buttonFontSize = 20.
    method textBoxFontSize = 20.
    method iconScale = 0.77
 	method windowCornerRadius = 2.
	method windowHeaderHeight               = 30.
	method windowDropShadowSize             = 10.
	method buttonCornerRadius               = 2.
	method tabBorderWidth                   = 0.75
	method tabInnerMargin                   = 5.
	method tabMinButtonWidth                = 20.
	method tabMaxButtonWidth                = 160.
	method tabControlWidth                  = 20.
	method tabButtonHorizontalPadding       = 10.
	method tabButtonVerticalPadding         = 2.

	method dropShadow                       = rgba 0 0 0 128
	method transparent                      = rgba 0 0 0 0
	method borderDark                       = rgba 29 29 29 255
	method borderLight                      = rgba 92 92 92 255
	method borderMedium                     = rgba 35 35 35 255
	method textColor                        = rgba 255 255 255 160
	method disabledTextColor                = rgba 255 255 255 80
	method textColorShadow                  = rgba 0 0 0 160
	method iconColor                        = self#textColor

	method buttonGradientTopFocused         = rgba 64 64 64 255
	method buttonGradientBotFocused         = rgba 48 48 48 255
	method buttonGradientTopUnfocused       = rgba 74 74 74 255
	method buttonGradientBotUnfocused       = rgba 58 58 58 255
	method buttonGradientTopPushed          = rgba 41 41 41 255
	method buttonGradientBotPushed          = rgba 29 29 29 255

	(* Window-related *)
	method windowFillUnfocused              = rgba 43 43 43 230
	method windowFillFocused                = rgba 45 45 45 230
	method windowTitleUnfocused             = rgba 220 220 220 160
	method windowTitleFocused               = rgba 255 255 255 190

	method windowHeaderGradientTop          = self#buttonGradientTopUnfocused
	method windowHeaderGradientBot          = self#buttonGradientBotUnfocused
	method windowHeaderSepTop               = self#borderLight
	method windowHeaderSepBot               = self#borderDark

	method windowPopup                      = rgba 50 50 50 255
	method windowPopupTransparent           = rgba 50 50 50 0

    (* Icons *)
	method checkBoxIcon                     = Entypo.check
    method messageInformationIcon           = Entypo.circled_info
    method messageQuestionIcon              = Entypo.circled_help
    method messageWarningIcon               = Entypo.warning
    method messageAltButtonIcon             = Entypo.circled_cross
    method messagePrimaryButtonIcon         = Entypo.check
    method popupChevronRightIcon            = Entypo.chevron_right
    method popupChevronLeftIcon             = Entypo.chevron_left
    method tabHeaderLeftIcon                = Entypo.left_bold
    method tabHeaderRightIcon               = Entypo.right_bold
    method textBoxUpIcon                    = Entypo.chevron_up
    method textBoxDownIcon                  = Entypo.chevron_down
end

type window = <
    size : Vec2.t;
>

module V = Containers.Vector

type mouse_button = int

type event = (ui_event, widget_spec) Event.event

and handlers = (ui_event, widget_spec) Handlers.t

and screen_spec = <
    pixelRatio : float;
    size : Vec2.t;
    updateFocus : widget_spec option -> unit; 
    glfwWindow : GLFW.window;
    addChild : int -> widget_spec -> unit;
    addChildWidget : widget_spec -> unit; 
    setLayoutDirty : unit;
    removeChildIndex : int -> unit;
    removeChildWidget : widget_spec -> unit; 
>
and layout = <
    performLayout : graphics_context -> widget_spec -> unit;
    preferredSize : graphics_context -> widget_spec -> Vec2.t;
>
and widget_spec = <
    id : string;
	absolutePosition : Vec2.t; 
    addChild : int -> widget_spec -> unit;
    addChildWidget : widget_spec -> unit; 
    handleEvent : event -> unit;
    toLocal : Vec2.t -> Vec2.t;
    asScreen : screen_spec option; 
    childAt : int -> widget_spec option; 
    childCount : int; childIndex : widget_spec -> int option; 
    children : widget_spec V.vector; 
    contains : Vec2.t -> bool;
    cursor : GLFW.cursor option; 
    draw : graphics_context -> unit; 
    enabled : bool; 
    fixedHeight : float;
    fixedSize : Vec2.t option; 
    fixedWidth : float;
    focused : bool; 
    fontSize :  float; 
    hasFontSize : bool; 
    hasParent : bool; 
    iconExtraScale : float; 
    layout : layout option;
    parent : widget_spec option; 
    performLayout : graphics_context -> unit;
    position : Vec2.t; 
    preferredSize : graphics_context -> Vec2.t;
    fixedOrPreferredSize : graphics_context -> Vec2.t;
    clearChildren : unit;
    removeChildIndex : int -> unit;
    removeChildWidget : widget_spec -> unit; 
    requestFocus : unit;
    screen : screen_spec option;
    setCursor : GLFW.cursor -> unit; 
    setEnabled : bool -> unit;
    setFixedSize : Vec2.t -> unit; 
    setFocused : bool -> unit;
    setFontSize : float -> unit; 
    setHeight : float -> unit;
    setIconExtraScale : float -> unit; 
    setId : string -> unit;
    setLayout : layout -> unit; 
    setParent : widget_spec -> unit;
    setPosition : Vec2.t -> unit; 
    setSize : Vec2.t -> unit;
    setTheme : theme -> unit; 
    setTooltip : string -> unit;
    setVisible : bool -> unit; 
    setWidth : float -> unit; 
    size :  Vec2.t; 
    theme : theme; 
    tooltip : string option; 
    visible : bool;
    visibleRecursive : bool; 
    width : float;
    height : float;
>

let swap v idx1 idx2 =
    let tmp = V.get v idx1 in
    let elem2 = V.get v idx2 in
    V.set v idx1 elem2;
    V.set v idx2 tmp;
;;

let remove_at v element =
    let len = V.length v in
    let rec loop idx =
        if idx >= len then ()
        else if phys_equal element V.(get v idx) then (
            V.remove_and_shift v idx
        ) else loop (idx+1)
    in
    loop 0
;;

let reverseIterVector ?(offset=0) vector fn =
        let rec loop idx =
            if idx < 0 then false
            else (
                let item = V.get vector idx in
                if fn item then true
                else loop (idx-1)
            )
        in
        loop (V.length vector - 1 - offset)
;;

let findi v fn =
    let rec loop idx =
        if idx >= V.length v then None
        else (
            if fn V.(get v idx) then Some (idx, v)
            else loop (idx+1)
        )
    in
    loop 0
;;

let insert_at v idx elem =
    V.push v elem;
    for i=V.length v - 1 downto idx+1 do
        swap v i (i-1)
    done;
    V.set v idx elem
;;

let global_filter : (ui_event, widget_spec) Handlers.t = Handlers.create()

let parent_chain (widget : widget_spec) =
    let rec loop acc w =
        match w#parent with
        | None -> w :: acc
        | Some p -> loop (w :: acc) p
    in
    loop [] widget
;;

let find_widget (root : widget_spec) (pt : Vec2.t) : widget_spec option * (widget_spec list) =
    (* TODO - add faster child intersection function *)
    let rec loop idx pt widget =
        if idx < 0 then 
            if widget#contains pt then (Some widget, [widget])
            else None, []
        else (
            let child = V.get widget#children idx in
            let child_pt = Vec2.(pt - widget#position) in
            if child#visible && child#contains child_pt then (
                let idx = V.length child#children - 1 in
                let target, path = loop idx child_pt child in
                target, widget :: path
            ) else loop (idx-1) pt widget 
        )
    in
    loop V.(length root#children - 1) (*Vec2.(pt - root#position)*) pt root
;;

let rec propagate acc (event : event) = function
    | [] -> acc
    | w :: rest ->
        w#handleEvent event;
        if event.stop_propagation 
        then acc
        else propagate (w :: acc) event rest
;;

let dispatch_event_with_chain (event : ui_event) (target : widget_spec) parent_chain =
    let down_event = Event.create event target Event.Down in
    (* Go down *)
    let up_chain = propagate [] down_event parent_chain in
    if not down_event.stop_propagation then (
        (* Go back up *)
        let up_event = Event.create event target Event.Up in
        propagate [] up_event up_chain |> ignore
    )
;;

let dispatch_event_by_position (root : widget_spec) (event : ui_event) (pos : Vec2.t) =
    let target, path = find_widget root pos in
    match target with
    | Some target -> dispatch_event_with_chain event target path
    | None -> ()

let dispatch_event (event : ui_event) (target : widget_spec) =
    let chain = parent_chain target in
    dispatch_event_with_chain event target chain
;;

let update_focus_paths (last, last_path) (curr, curr_path) p =
    (* Update mouse focus *)
    begin match last, curr with
    | None, None
    | None, Some _
    | Some _, None -> ()
    | Some last, Some curr ->
        if last != curr then (
            dispatch_event_with_chain (MouseEnter {pos=p; entered=false}) last last_path;
            dispatch_event_with_chain (MouseEnter {pos=p; entered=true}) curr curr_path;
        )
    end
;;

let update_mouse_focus root p rel : unit =
    (* Check last control under cursor *)
    let last = find_widget (root :> widget_spec) Vec2.(p - rel) in
    let curr = find_widget (root :> widget_spec) p in
    update_focus_paths last curr p
;;

(* Add a ctx argument? *)
class widget parent = object(self)
    val mutable parent : widget option = parent
    val mutable layout : layout option = None
    val mutable theme : theme = new theme
    val mutable position : Vec2.t = Vec2.zero
    val mutable size : Vec2.t = Vec2.zero
    val mutable fixed_size : Vec2.t option = None
    val mutable visible : bool = true
    val mutable children : widget V.vector = V.create()
    val mutable id : string = ""
    val mutable enabled : bool = true
    val mutable focused : bool = false
    val mutable tooltip : string option = None
    val mutable fontSize : float option = None
    val mutable iconExtraScale : float = 1.0
    val mutable cursor : GLFW.cursor option = None
    val mutable mouseFocus : bool = false
    val mutable userHandler : handlers = Handlers.create()

    initializer
        match parent with
        | None -> ()
        | Some p -> p#addChildWidget (self :> widget)

    method parent = parent
    method setParent p = parent <- Some p

    method layout = layout
    method setLayout l = layout <- Some l

    method theme = theme
    method setTheme t =
        if phys_equal t theme then ()
        else (
            V.iter (fun child -> child#setTheme t) children
        )

    method position = position
    method setPosition p = position <- p

    method absolutePosition =
        match parent with
        | None -> position
        | Some p -> Vec2.(p#absolutePosition + position)

    method size = size
    method setSize s = size <- s

    method width = size.a
    method setWidth w = size <- Vec2.set_a size w

    method height = size.b
    method setHeight h = size <- Vec2.set_b size h

    method setFixedSize s = 
        self#markLayoutDirty;
        fixed_size <- Some s

    method fixedSize = fixed_size

    method fixedWidth = 
        match fixed_size with
        | None -> 0.
        | Some fs -> fs.a

    method fixedHeight = 
        match fixed_size with
        | None -> 0.
        | Some fs -> fs.b

    method visible = visible
    method setVisible v = visible <- v

    method visibleRecursive =
        let rec loop visible = function
            | None -> visible
            | Some (w : widget) ->
                let visible = w#visible && visible in
                loop visible w#parent
        in
        loop true (Some (self :> widget))

    method childCount = V.length children

    method addChildWidget (child : widget) : unit =
        self#addChild (V.length children) child

    method addChild (index : int) (child : widget) : unit =
        assert (child != (self :> widget));
        insert_at children index child;
        child#setParent (self :> widget);
        child#setTheme theme;

    method clearChildren =
        V.clear children

    method removeChildIndex (index : int) : unit =
        V.remove_and_shift children index

    method removeChildWidget (widget : widget) : unit =
        remove_at children widget

    method childAt (index : int) : widget option =
        if index >= 0 && index < V.length children then (
            Some (V.get children index)
        ) else None

    method asScreen : screen_spec option =
        None

    method children = children

    method toLocal (pt : Vec2.t) =
        let chain = parent_chain (self :> widget) in
        let rec loop pt = function
            | [] | [_] -> pt
            | p :: tl -> loop Vec2.(pt - p#position) tl
        in
        loop pt chain

    method private reverseIterChildren fn =
        let rec loop idx =
            if idx < 0 then false
            else (
                let child = V.get children idx in
                if not child#visible then loop (idx-1)
                else if fn child then true
                else loop (idx-1)
            )
        in
        loop (V.length children - 1)

    method contains (p : Vec2.t) : bool =
        let diff = Vec2.(p - position) in
        (diff.a >= 0. 
         && diff.b >= 0.
         && diff.a < size.a
         && diff.b < size.b)

    method private mouseMotionEvent ~(pos:Vec2.t) ~(rel:Vec2.t) (_mouse_button : mouse_button) (_modifiers : modifiers) =
        ignore(pos);
        ignore(rel);
        false
 
    method childIndex (widget : widget) : int option =
        let idx = ref ~-1 in
        let found = V.exists (fun child ->
            incr idx;
            phys_equal widget child
        ) children in
        if found then Some !idx
        else None

    method screen : screen_spec option =
        let rec loop widget =
            match widget#asScreen with
            | Some _ as s -> s
            | None ->
                match widget#parent with
                | None -> (*failwith "Must have screen"*) None
                | Some p -> loop p
        in
        loop (self :> widget)

    method setId new_id = id <- new_id
    method id = id

    method enabled = enabled
    method setEnabled e = enabled <- e

    method focused = focused
    method setFocused b = focused <- b

    method hasParent = Option.is_some parent

    method requestFocus : unit =
        match self#screen with
        | None -> failwith "No screen?"
        | Some screen ->
            screen#updateFocus (Some (self :> widget_spec))

    method tooltip = tooltip
    method setTooltip t = tooltip <- Some t

    method hasFontSize = Option.is_some fontSize

    method fontSize =
        match fontSize with
        | None -> theme#standardFontSize
        | Some v -> v

    method setFontSize s = fontSize <- Some s

    method iconExtraScale = iconExtraScale
    method setIconExtraScale s = iconExtraScale <- s

    method cursor = cursor
    method setCursor c = cursor <- Some c

    method private mouseButtonEvent ~(pos:Vec2.t) (_button : int) (_down : bool) (_mods : modifiers) =
        ignore(pos);
        false

    method private scrollEvent ~(pos:Vec2.t) ~(rel:Vec2.t) =
        ignore(pos);
        ignore(rel);
        false

    method private mouseDragEvent ~(pos:Vec2.t) ~(rel:Vec2.t) (_mouse_button : mouse_button) (_modifiers : modifiers) = 
        ignore(pos);
        ignore(rel);
        false

    method private mouseEnterEvent ~(pos:Vec2.t) ~(entered:bool) =
        ignore(entered);
        ignore(pos);
        false

    method private focusEvent ~(focus:bool) =
        ignore(focus);
        false

    method private keyboardEvent ~(key: GLFW.key) ~(scancode:int) ~(action:GLFW.key_action) (modifiers : GLFW.key_mod list) =
        ignore(key);
        ignore(scancode);
        ignore(action);
        ignore(modifiers);
        false

    method private keyboardCharacterEvent (_ : int) = false

    method private markLayoutDirty =
        match self#screen with
        | Some sc -> sc#setLayoutDirty
        | None -> ()

    method fixedOrPreferredSize ctx =
        match fixed_size with
        | None -> self#preferredSize ctx
        | Some fs -> fs

    method preferredSize (ctx : graphics_context) : Vec2.t =
        match layout with
        | None -> size
        | Some l -> l#preferredSize ctx (self :> widget_spec)

    method performLayout (ctx : graphics_context) : unit =
        match layout with
        | Some l -> l#performLayout ctx (self :> widget_spec)
        | None ->
            V.iter (fun child ->
                child#setSize (child#fixedOrPreferredSize ctx);
                child#performLayout ctx
            ) children

    method draw (nvg : graphics_context) =
        let open Gv in
        (*
        stroke_width nvg 1.;
        begin_path nvg;
        rect nvg 0.5 0.5 (size.a-.1.) (size.b-.1.);
        stroke_color nvg (rgba 255 0 0 255);
        stroke nvg;
        *)

        if V.is_empty children then ()
        else (
            save nvg;
            (*translate nvg position.a position.b;*)
            V.iter (fun child ->
                (* TODO - and check if child intersects current scissor ? rect... *)
                if child#visible then (
                    save nvg;
                    let cpos : Vec2.t = child#position in
                    let csize : Vec2.t = child#size in
                    Gv.Scissor.intersect nvg ~x:cpos.a ~y:cpos.b ~w:csize.a ~h:csize.b;

                    (*
                    begin_path nvg;
                    rect nvg cpos.a cpos.b csize.a csize.b;
                    stroke_color nvg (rgba 255 0 0 255);
                    stroke nvg;
                    *)
                    
                    Gv.Transform.translate nvg ~x:cpos.a ~y:cpos.b;
                    child#draw nvg;
                    restore nvg;
                )
            ) children;
            restore nvg;
        )

    method private handleEventInternal (event : event) =
        match event.value with
        | Focus focus ->
                focused <- focus;
                if self#focusEvent ~focus then (
                    Event.stop event
                )

        | MouseEnter {entered; pos} when event.phase = Up ->
                mouseFocus <- entered;
                if self#mouseEnterEvent ~pos ~entered then (
                    Event.stop event
                )

        | ScrollEvent {pos; rel} when event.phase = Up -> 
                if self#scrollEvent ~pos:(self#toLocal pos) ~rel then (
                    Event.stop event
                )

        | MouseDrag {pos; rel; button; mods} when event.phase = Up ->
                if self#mouseDragEvent ~pos:(self#toLocal pos) ~rel button mods then (
                    Event.stop event
                )

        | MouseMotion {pos; rel; button; mods} when event.phase = Up ->
                let offset = self#toLocal pos in
                if self#mouseMotionEvent ~pos:offset ~rel button mods then (
                    Event.stop event
                )

        | MouseButton {pos; button; down; mods} when event.phase = Up ->
            begin match self#mouseButtonEvent ~pos:(self#toLocal pos) button down mods with
            | true -> Event.stop event
            | false ->
                if (button = GLFW.mouse_button_left) && down && not focused then (
                    self#requestFocus
                );
            end

        | KeyboardChar codepoint -> 
            if self#keyboardCharacterEvent codepoint then (
                Event.stop event
            );
            
        | KeyboardKey {key; scancode; action; mods} ->
            if self#keyboardEvent ~key ~scancode ~action mods then (
                Event.stop event
            );

        | _ -> ()

    method handleEvent (event : event) =
        (* TODO - maybe encapsulate in a function
         * handle user_handler self_handler
         *)
        Handlers.invoke userHandler event;
        if not event.stop_immediate && not event.default_prevented then (
            self#handleEventInternal event
        )

end

let dummy_widget = new widget None
