open! Widget
open! Button

class pushbutton parent caption = object(self)
    inherit button parent caption NoImage

    val mutable changeCallback : bool -> unit = (fun _ -> ())

    method changeCallback = changeCallback
    method setChangeCallback c = changeCallback <- c

    method isSet = self#pushed
    method set = self#setPushed

    method! mouseButtonEvent ~pos button down _mods =
        let push_backup = pushed in
        if (button = GLFW.mouse_button_left) && enabled then (
            if down then (
                pushed <- not pushed;
            ) else if pushed then (
                if self#contains pos then (
                    callback();
                );
            );

            if (push_backup <> pushed) then (
                changeCallback pushed;
            );

            true
        ) else false
end
