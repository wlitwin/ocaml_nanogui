open! Widget

open Pushbutton
open Popup
open Listbox

class dropdown parent = object(self)
    inherit pushbutton parent "No selection" as _super

    val listbox : listbox = new listbox None
    val popup : popup = new popup dummy_widget
    val mutable selectionCallback : int option -> unit = (fun _ -> ())

    (*TODO - hide these from the parent inheritance *)
    method! private callback = assert false
    method! private setCallback = assert false

    method selectionCallback = selectionCallback
    method setSelectionCallback s = selectionCallback <- s

    method! setPushed _p = ()

    initializer 
        self#setIcon (FontIcon Entypo.chevron_down);
        self#setIconPosition Right;
        popup#setAnchor (self :> widget);
        popup#setDraggable false;
        popup#addChildWidget (listbox :> widget);
        popup#setSide Bottom;
        listbox#setId "listbox_dropdown";
        listbox#setAlwaysOneSelection true;
        listbox#setCallback (fun idx ->
            self#updateState false;
            begin match idx with
            | None -> self#setCaption "No selection"
            | Some idx ->
                self#setCaption listbox#choices.(idx);
            end;
            selectionCallback idx
        )

    method private updateState value =
        pushed <- value;
        popup#setVisible value;
        self#setIcon (FontIcon
            (if value then Entypo.chevron_up
            else Entypo.chevron_down)
        )

    method! focusEvent ~focus =
        if not focus then (
            self#updateState false;
        );
        true

    method! performLayout ctx =
        let _lb_size = listbox#preferredSize ctx in
        let popup_size = Vec2.(mk self#size.a 100.) in
        popup#setFixedSize popup_size;
        popup#setSize popup_size;
        popup#performLayout ctx

    method! mouseButtonEvent ~pos:_ button down _mods =
        if button = Mouse.button_left && enabled then (
            if down then (
                if not pushed then (
                    self#requestFocus;
                );
                self#updateState (not pushed);
                true
            ) else false
        ) else false

    method choices = listbox#choices
    method setChoices c = 
        listbox#setChoices c
end
