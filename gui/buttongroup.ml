open! Widget

type callback = bool -> unit

class buttongroup = object(self)
    val mutable buttons : (togglable * callback) V.vector = V.create()
    val mutable alwaysOneSelected : bool = false

    method alwaysOneSelection = alwaysOneSelected
    method setAlwaysOneSelection a = alwaysOneSelected <- a

    method private unsetAllBut index : unit =
        V.iteri (fun idx (item, cb) ->
            if idx <> index then (
                item#set false;
                cb false;
            )
        ) buttons

    method setItem (index : int) =
        self#unsetAllBut index;
        V.(get buttons index |> fst)#set true

    method addItem (item : togglable) =
        V.push buttons (item, item#changeCallback);
        let index = V.length buttons - 1 in 
        item#setChangeCallback (fun b ->
            if b then (
                (V.(get buttons index) |> snd) b;
                self#unsetAllBut index
            ) else if alwaysOneSelected then (
                item#set true 
            )
        )

    method clearButtons =
        V.iter (fun (tg, cb) ->
            tg#setChangeCallback cb;
        ) buttons;
        V.clear buttons

    method removeItem (_item : togglable) =
        () (* TODO *)
end
