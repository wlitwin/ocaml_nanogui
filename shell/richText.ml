open Ocaml_nanogui_async
open Widget

type style = {
    color : Gv.Color.t;
    background : Gv.Color.t;
    size : float;
}

type text_info = {
    style : style;
    text : string;
    width : float;
}

type run = Text of text_info
         | Element of widget


type line = run list

class richText parent = object(self)
    inherit widget parent as super

    (* Have text runs + structurally special elements?
     *)
end
