open Ocaml_nanogui_async
(*open Bonsai_sdl*)
open Widget
open! Core_kernel

open Screen 
open Button 
open BoxLayout 
open Textbox 
open Slider 
open Colorwheel 
open Glcanvas 
open Label 
open Graph 
open Progressbar 
open Stackedwidget 
open Tabwidget 
open Scrollpanel 
open Pushbutton 
open Checkbox 
open Buttongroup 
open Popup 
open ConstraintLayout
open Listbox
open Dropdown
open Knob
open Imageview
open Multitextbox

let _ =
    let app = Application.init {
        mono_font = "./gui/UbuntuMono-Regular.ttf";
        icon_font = "./gui/entypo.ttf";
    } in
    let screen1 = Application.create_screen app ~title:"NanoGUI" ~width:400 ~height:400 in
    screen1#setId "screen1";

    let sparent = Some (screen1 :> widget) in

    let layout_lbl = new label sparent "Layout - 0.000ms" 18. in
    layout_lbl#setId "layout_lbl";
    layout_lbl#setTooltip "Amount of time it took last layout";

    let btn_ok = new button sparent "OK!" NoImage in
    btn_ok#setCallback (fun () ->
        print_endline "Clicked!";
    );

    let deps = Build.[
        lift btn_ok
        |> tl_size (wLeft 10.) (wTop 10.) (const 100.) (preferredH btn_ok);

        lift layout_lbl
        |> tr_pref_size (wRight ~-.10.) (wTop 10.)
    ] in

    screen1#setLayout (new constraintLayout deps);

    Application.run [screen1] ~idle:(fun () ->
        let render = !Application.last_render_time in
        layout_lbl#setText (Printf.sprintf "Layout %.3fms" (screen1#lastLayoutTime +. render));
    );
;;
