open Ocaml_nanogui_async
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
open Vertscroll

class textLayout label = object(self)
    val label : Label.label = label 

    method private calcSize ctx (widget : widget) =
        let width = widget#size.a in
        Gv.Text.set_align ctx ~align:Gv.Align.(left lor top);
        let b = Gv.Text.box_bounds ctx ~x:0. ~y:0. ~break_width:width label#text in
        Vec2.(mk width (b.ymax -. b.ymin))

    method preferredSize (ctx : graphics_context) (widget : widget) =
        self#calcSize ctx widget
        
    method performLayout (ctx : graphics_context) (widget : widget) =
        let size = self#calcSize ctx widget in
        let child = V.get widget#children 0 in
        child#setSize size
end

class paragraph parent text font_size = object(self)
    inherit label parent text font_size as super

    method! preferredSize ctx =
        (* Layout problem with scroll container, need to pass down extra info
           when the scrollbars are shown... this causes the text to be laid out
           again... which can cause some reflow issues
         *)
        match self#parent with
        | None -> Vec2.mk 100. 100.
        | Some parent ->
            let width = parent#size.a -. 12. in
            Gv.Text.set_align ctx ~align:Gv.Align.(left lor top);
            let break_width = width in
            let b = Gv.Text.box_bounds ctx ~x:0. ~y:0. ~break_width self#text in
            let sz = Vec2.(mk break_width (b.ymax -. b.ymin)) in
            sz

    method! draw ctx =
        super#draw ctx;
        let open Gv in
        Path.begin_ ctx;
        Path.rect ctx ~x:0. ~y:0. ~w:(size.a-.1.) ~h:(size.b-.1.);
        set_stroke_color ctx ~color:Color.white;
        stroke ctx;
end

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

    let render_lbl = new label sparent "Render - 0.000ms" 18. in
    layout_lbl#setId "render_lbl";
    layout_lbl#setTooltip "Amount of time it took last render";

    let scroll = new scrollpanel sparent in

    let label = new paragraph (Some (scroll :> widget)) Text.long_text 18. in
    label#setMultiline true;

    let btn_ok = new button sparent "OK!" NoImage in
    btn_ok#setCallback (fun () ->
        print_endline "Clicked!";
    );

    let deps = Build.[
        lift btn_ok
        |> tl_size (wLeft 10.) (wTop 10.) (const 100.) (preferredH btn_ok);

        lift layout_lbl
        |> tr_pref_size (wRight ~-.10.) (wTop 10.);

        lift render_lbl
        |> tr_pref_size (wRight ~-.10.) (bottomOf layout_lbl 10.);

        lift scroll
        |> top_left (leftOf btn_ok 10.) (bottomOf btn_ok 10.) 
        |> bottom_right (wRight ~-.10.) (wBottom ~-.10.)
        ;
    ] in

    screen1#setLayout (new constraintLayout deps);

    let total_render_time = ref 0. in
    let fps_counter = PerfCounter.create ~report_fn:(fun ~events ~avg ->
        let avg_frame = !total_render_time /. float events in
        let str = Printf.sprintf "FPS %d (%.2fms) Avg %.2fms" events avg (avg_frame*.1000.) in
        render_lbl#setText str;
        total_render_time := 0.;
    ) ~report_sec:1.
    in

    Application.run [screen1] ~idle:(fun () ->
        PerfCounter.mark_event fps_counter;
        total_render_time := !total_render_time +. !Application.last_render_time;
        layout_lbl#setText (Printf.sprintf "Layout %.3fms" screen1#lastLayoutTime);
    );
;;
