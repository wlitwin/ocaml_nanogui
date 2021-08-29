open! Widget
open! Scrollpanel
open! BoxLayout
open! Buttongroup
open! Label 
open! Vboxlayout

class choice parent value = object(_self)
    inherit label parent value 20. as super
    inherit togglableImpl

    val mutable selected : bool = false
    val mutable changeCallback : bool -> unit = (fun _ -> ())

    initializer
        id <- "choice"

    method! mouseButtonEvent ~pos:_ button down _mods = 
        if button = GLFW.mouse_button_left && down then (
            selected <- not selected;
            changeCallback selected;
            true
        ) else false

    method! preferredSize ctx =
        super#preferredSize ctx

    method! draw ctx =
        super#draw ctx;
        let open Gv in
        
        if selected || mouseFocus then (
            let color = 
                if selected 
                then rgba 128 128 128 128
                else rgba 164 164 164 128
            in
            Path.begin_ ctx;
            Path.rect ctx ~x:0. ~y:0. ~w:size.a ~h:size.b;
            set_fill_color ctx ~color;
            fill ctx;
        )
end

class listbox parent = object(self)
    inherit widget parent as _super

    val mutable choices : string array = [||]
    val mutable selectedIndex : int option = None
    val buttongroup = new buttongroup
    val scroll = new scrollpanel None
    val scrollChild = new widget None
    val mutable callback : int option -> unit = (fun _ -> ())

    method selectedIndex = selectedIndex
    method setSelectedIndex index =
        if index >= 0 && index < Array.length choices then
            selectedIndex <- Some index

    method callback = callback
    method setCallback c = callback <- c

    method alwaysOneSelection = buttongroup#alwaysOneSelection
    method setAlwaysOneSelection a = buttongroup#setAlwaysOneSelection a

    initializer
        self#setLayout (new FullLayout.fullLayout);
        self#addChildWidget (scroll :> widget);
        scroll#setId "scrollpanel_listbox";
        scroll#addChildWidget (scrollChild :> widget);
        scrollChild#setId "scroll_listbox_child";
        (*self#addChildWidget (scrollChild :> widget);*)
        scrollChild#setLayout (new boxLayout Vertical Fill 0. 0. :> layout);

    method! preferredSize ctx =
        scrollChild#preferredSize ctx

    method choices = choices
    method setChoices c = 
        choices <- c;
        buttongroup#clearButtons;
        scrollChild#clearChildren;
        Array.iteri (fun idx ch ->
            let item = new choice (Some (scrollChild :> widget)) ch in
            item#setChangeCallback (fun selected ->
                begin match selectedIndex with
                | None ->
                    if selected then (
                        selectedIndex <- Some idx;
                    )
                | Some v ->
                    if selected then (
                        selectedIndex <- Some idx
                    ) else (
                        if v = idx && not self#alwaysOneSelection then (
                            selectedIndex <- None
                        )
                    )
                end;
                callback selectedIndex
            );
            buttongroup#addItem (item :> togglable);
        ) c;
end
