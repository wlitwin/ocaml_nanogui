open Ocaml_nanogui
open Widget
open Button

open! Screen 
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

let clamp min max v =
    if v < min then min
    else if v > max then max
    else v

let graph_popup_widget parent text = 
    let pb = new pushbutton (Some (parent :> widget)) text in
    let popup = new popup (pb :> widget) in
    popup#setId "POPUP!";
    pb#setCallback (fun () -> 
        Printf.printf "Pushbutton clicked\n%!"
    );
    pb#setChangeCallback (fun b -> 
        Printf.printf "Pushbutton change %b\n%!" b;
        popup#setVisible b;
    );
    let button_bar = new widget (Some (popup :> widget)) in
    button_bar#setLayout (new boxLayout Horizontal Middle 5. 5. :> layout);
    button_bar#setId "Button_bar";
    let btn_close = new button (Some (button_bar :> widget)) "X" NoImage in
    let filler = new widget (Some (button_bar :> widget)) in
    let btn_change_fill = new button (Some (button_bar :> widget)) "C" NoImage in
    filler#setFixedSize Vec2.(mk 50. 1.);
    btn_change_fill#setFixedSize Vec2.(mk 30. 20.);
    btn_close#setFixedSize Vec2.(mk 30. 20.);
    btn_close#setCallback (fun () -> 
        popup#setVisible false;
        pb#setPushed false;
    );
    let graph2 = new graph (Some (popup :> widget)) in
    btn_change_fill#setCallback (fun () ->
        graph2#setFillUnderArea (not graph2#fillUnderArea);
    );
    graph2#setId "Graph 2";

    let speed = ref 1. in
    let quant = ref false in
    let arr = Array.make 200 0. in
    let data2 () =
        let dx = 2.*.Float.pi /. 200. in
        let time = Time.now() in
        Array.iteri (fun idx _ ->
            let i = 
                if !quant 
                then Float.round (Float.of_int idx /. 10.) *. 10.
                else Float.of_int idx 
            in
            let v =
                ((sin (i*.2.*.dx +. time*.2.*.(!speed))) /. 2. +. 0.5)
                +. (sin (i*.20.*.dx +. time))*.0.1
            in
            arr.(idx) <- v
        ) arr
    in
    data2();
    popup#setLayout (new boxLayout Vertical Fill 5. 5. :> layout);
    popup#setSide Bottom;
    graph2#setData arr;
    graph2#setHeader "Sin(2x)";
    graph2#setFooter "time";
    graph2#setCaption "Caption";
    graph2#setFixedSize Vec2.(mk 350. 200.);
    let horz = new widget (Some (popup :> widget)) in
    horz#setId "horz";
    horz#setLayout (new boxLayout Horizontal Fill 5. 5. :> layout);
    let btn_dec = new button (Some (horz :> widget)) "Slower" NoImage in
    let btn_inc = new button (Some (horz :> widget)) "Faster" NoImage in
    let btn_q = new button (Some (horz :> widget)) "Q" NoImage in
    btn_q#setCallback (fun () -> quant := not !quant);
    let btn_L = new button (Some (horz :> widget)) "L" NoImage in
    let btn_R = new button (Some (horz :> widget)) "R" NoImage in
    let btn_T = new button (Some (horz :> widget)) "T" NoImage in
    let btn_B = new button (Some (horz :> widget)) "B" NoImage in
    btn_L#setCallback (fun () -> popup#setSide Left);
    btn_R#setCallback (fun () -> popup#setSide Right);
    btn_T#setCallback (fun () -> popup#setSide Top);
    btn_B#setCallback (fun () -> popup#setSide Bottom);
    btn_inc#setCallback (fun () ->
        speed := clamp 1. 5. (!speed +. 1.)
    );
    btn_dec#setCallback (fun () ->
        speed := clamp 1. 5. (!speed -. 1.)
    );
    data2, pb, graph2
;;


let go () =
    let app = Application.init { mono_font = "consolas"; icon_font = "arial" } in

    let screen1 = Application.create_screen app ~title:"NanoGUI" ~width:1000 ~height:1000 in
    screen1#setId "screen1";

    let sparent = Some (screen1 :> widget) in

    let render_lbl = new label sparent  "RENDER 0.00ms" 24. in
    render_lbl#setId "render_lbl";
    render_lbl#setTooltip "Average render time over the last 1s";

    let fps_lbl = new label sparent "FPS 00 - avg 0.00ms" 24. in
    fps_lbl#setId "fps_lbl";
    fps_lbl#setTooltip "Average fps over the last 1s";

    let layout_lbl = new label sparent "Layout - 0.000ms" 24. in
    layout_lbl#setId "layout_lbl";
    layout_lbl#setTooltip "Amount of time it took last layout";

    let tabwidget = new tabwidget sparent in
    tabwidget#setId "tab widget";

    let btn_container = new widget sparent in
    btn_container#setId "buttons";
    btn_container#setLayout (new boxLayout Horizontal Middle 5. 5. :> layout);

    let bparent = Some (btn_container :> widget) in
    let btn1 = new button bparent "Button 1" (FontIcon Entypo.traffic_cone) in
    btn1#setIconPosition Right;
    btn1#setTooltip "Button 1 Tooltip!";
    btn1#setId "btn1";

    let btn2 = new button bparent "Button 2 loooooong" (FontIcon Entypo.search) in
    btn2#setId "btn2";
    btn2#setTooltip "Tool tip!";

    let btn3 = new button bparent "Button 3" NoImage in
    btn3#setId "btn3";
    btn3#setTooltip "Another tooltip, super cool!";

    let tab1 = tabwidget#createTab "Group" in
    tab1#setId "tab1";
    tab1#setLayout (new boxLayout Vertical Middle 5. 5. :> layout);
    let progress1 = new progressbar (Some (tab1 :> widget)) in
    progress1#setId "progress1";
    let checkbox1 = new checkbox (Some (tab1 :> widget)) "Checkbox!" in
    let checkbox2 = new checkbox (Some (tab1 :> widget)) "Checkbox 2!" in
    checkbox1#setChangeCallback (fun b -> Printf.printf "Checkbox %b\n%!" b);
    checkbox2#setChangeCallback (fun b -> Printf.printf "Checkbox 2 %b\n%!" b);
    let pb3 = new pushbutton (Some (tab1 :> widget)) "Bop It!" in
    let pb2 = new pushbutton (Some (tab1 :> widget)) "Push It!" in
    pb2#setCallback (fun () -> Printf.printf "Pushbutton TWO clicked\n%!");
    pb2#setChangeCallback (fun b -> Printf.printf "Pushbutton TWO change %b\n%!" b);
    pb3#setCallback (fun () -> Printf.printf "Pushbutton THREE clicked\n%!");
    pb3#setChangeCallback (fun b -> Printf.printf "Pushbutton THREE change %b\n%!" b);
    let bg = new buttongroup in
    bg#addItem (checkbox1 :> togglable);
    bg#addItem (checkbox2 :> togglable);
    bg#addItem (pb2 :> togglable);
    bg#addItem (pb3 :> togglable);

    let tab2 = tabwidget#createTab "Color" in
    tab2#setId "tab2";
    tab2#setLayout (new boxLayout Vertical Middle 5. 5. :> layout);
    let color = new colorwheel (Some (tab2 :> widget)) (1., 0.5, 0.) in
    color#setId "Color";

    (*
    let vscroll2 = new scrollpanel (*(Some (tab1 :> widget)) *) sparent in
    let glcanvas2 = new glcanvas (Some (vscroll2 :> widget)) in
    glcanvas2#setFixedSize Vec2.(mk 300. 300.);
    glcanvas2#setDrawCallback (draw_triangle 0.5);
    *)

    let stacked = new stackedwidget (Some (screen1 :> widget)) in
    stacked#setId "Stacked";
    btn1#setCallback (fun () ->
        Caml.print_endline "Button1 clicked!";
        stacked#setSelectedIndex 0;
    );
    btn2#setCallback (fun () ->
        Caml.print_endline "Button2 clicked!";
        stacked#setSelectedIndex 1;
    );
    btn3#setCallback (fun () ->
        Caml.print_endline "Button3 clicked!";
        stacked#setSelectedIndex 2;
    );
    let hbox = new widget (Some (stacked :> widget)) in 
    hbox#setLayout (new boxLayout Horizontal Middle 5. 5. :> layout);
    let slider1 = new slider (Some (hbox :> widget)) ~min:0. ~max:100. ~value:50. in
    let label2 = new label (Some (hbox :> widget)) "50" 20. in
    slider1#setFixedSize Vec2.(mk 150. 25.);
    slider1#setCallback (fun v ->
        label2#setText Printf.(sprintf "%.0f" v);
    );
    let graph1 = new graph (Some (stacked :> widget)) in
    graph1#setId "graph1";
    let tbox = new textbox (Some (stacked :> widget)) "Hello!" in
    tbox#setId "tbox";
    let data = 
        let dx = 2.*.Float.pi /. 200. in
        Array.init 200 (fun i ->
            let i = Float.of_int i in
            Float.((sin (i*.dx)) /. 2. +. 0.5)
        )
    in
    graph1#setData data;
    graph1#setHeader "Sin(x)";
    graph1#setFooter "time";
    graph1#setCaption "Caption";

    (*
    let glcanvas2 = new glcanvas (Some (screen1 :> widget)) in
    glcanvas2#setFixedSize Vec2.(mk 300. 300.);
    glcanvas2#setDrawCallback (draw_triangle 0.);
    *)

    let tab3 = tabwidget#createTab "tab3" in
    tab3#setLayout (new FullLayout.fullLayout);

    (*
    let scroll2 = new scrollpanel (Some (tab3 :> widget)) in
    (*scroll2#setFixedSize Vec2.(mk 100. 100.);*)
    let glcanvas3 = new glcanvas (Some (scroll2 :> widget)) in
    glcanvas3#setId "glcanvas3";
    glcanvas3#setFixedSize Vec2.(mk 300. 300.);
    glcanvas3#setDrawCallback (draw_triangle 0.25);
    *)

    tabwidget#setActiveTab 1;

    let data1, pb1, _ = graph_popup_widget screen1 "Graph 1!" in
    let data2, pb2, _ = graph_popup_widget screen1 "Graph 2!" in

    let listbox = new listbox sparent in
    listbox#setId "listbox";
    listbox#setChoices [|
        "Choice 1"; 
        "Another choice"; 
        "choice choice"; 
        "last item!";
        "Another choice!";
        "Super duper choice!";
        "More choices";
        "And even more!";
    |];

    let lbl_dropdown = new label sparent "Make a selection" 20. in
    lbl_dropdown#setId "lbl_dropdown";

    let dropdown = new dropdown sparent in
    dropdown#setId "dropdown";
    dropdown#setChoices [|
        "Choice A";
        "Choice B";
        "Choice C";
        "Choice D";
        "Choice E";
        "Choice F";
        "Choice G";
        "Choice H";
        "Choice I";
        "Choice J";
        "Choice K";
        "Choice L";
    |];
    dropdown#setSelectionCallback (function
        | None -> lbl_dropdown#setText "Make a selection"
        | Some idx -> lbl_dropdown#setText (dropdown#choices.(idx) ^ " - Nice!")
    );

    let knob = new knob sparent ~min:0. ~max:100. ~value:50. in
    knob#setId "knob";

    let knob2 = new knob sparent ~min:0. ~max:100. ~value:75. in
    knob2#setId "knob 2";

    (*let img = Gv.Image.create screen1#nvgContext "./bin/turtle.png" 0 in*)
    let img = Gv.Image.from_color screen1#nvgContext
        ~width:1
        ~height:1
        ~data:[|Gv.Color.rgb ~r:255 ~g:255 ~b:0|]
        ~flags:Gv.ImageFlags.no_flags
        |> Stdlib.Option.get
    in
    let imgview = new imageview sparent img in
    imgview#setId "imageview";

    let set_img_size value =
        let value = value /. 100. in
        let scale = 100. *. value +. 1. in
        imgview#setFixedSize Vec2.(mk1 scale);
    in

    knob#setCallback set_img_size;

    set_img_size knob#value;

    let mtbox = new multilineTextbox sparent in
    mtbox#setId "multi-text-box";

    let pad = 10. in
    let deps = Build.[
        lift fps_lbl
        |> tr_pref_size (wRight ~-.pad) (wTop pad)
        ;

        lift render_lbl
        |> tr_pref_size (wRight ~-.pad) (bottomOf fps_lbl pad)
        ;

        (*
        lift layout_lbl
        |> tr_size (rightOf vscroll2 0.) (bottomOf vscroll2 pad) (const 175.) (const 24.)
        ;
        *)

        lift btn_container
        |> tl_pref_size (wLeft pad) (wTop pad)
        ;

        lift tabwidget
        |> tl_size (wLeft pad) (bottomOf btn_container pad) (const 200.) (const 200.)
        ;

        (*
        lift vscroll2
        |> tr_size (wRight ~-.pad) (bottomOf render_lbl pad) (const 200.) (const 200.)
        ;
        *)

        lift stacked
        |> tl_pref_size (rightOf tabwidget pad) (topOf tabwidget 0.)
        ;

        lift pb1
        |> tl_pref_size (leftOf stacked 0.) (bottomOf stacked pad)
        ;
        
        lift pb2
        |> tl_pref_size (leftOf pb1 0.) (bottomOf pb1 pad)
        ;

        (*
        lift glcanvas2
        |> tr_size (leftOf vscroll2 ~-.pad) (topOf vscroll2 0.) (const 100.) (const 100.)
        ;
        *)

        lift listbox
        |> tl_size (leftOf tabwidget 0.) (bottomOf tabwidget pad) (prefW_no_dep listbox) (const 100.)
        (*(const 100.) (const 200.)*)
        ;

        lift lbl_dropdown 
        |> tl_pref_size (rightOf listbox pad) (topOf listbox 0.)
        ;

        lift dropdown
        |> tl_pref_size (rightOf listbox pad) (bottomOf lbl_dropdown pad)
        ;

        lift knob
        |> tl_pref_size (leftOf tabwidget 0.) (bottomOf listbox pad)
        ;

        lift knob2
        |> tl_pref_size (rightOf knob pad) (topOf knob 0.)
        ;

        lift imgview
        |> tl_pref_size (rightOf knob2 pad) (topOf knob2 0.)
        ;

        lift mtbox
        |> tl_size (rightOf knob 0.) (bottomOf knob pad) (const 100.) (const 200.)
        ;

    ] in

    screen1#setLayout (new constraintLayout deps);

    let total_render_time = ref 0. in
    let fps_counter = PerfCounter.create ~report_fn:(fun ~events ~avg ->
        let str = Printf.sprintf "FPS %d - avg %.2fms" events avg in
        fps_lbl#setText str;
        let avg_frame = !total_render_time /. float events in
        let str = Printf.sprintf "TOTAL RENDER %.2fms" (avg_frame *. 1000.) in
        render_lbl#setText str;
        total_render_time := 0.;
    ) ~report_sec:1.
    in

    Application.run app [screen1] ~idle:(fun () ->
        PerfCounter.mark_event fps_counter;
        total_render_time := !total_render_time (*+. !Application.last_render_time*);
        layout_lbl#setText (Printf.sprintf "Layout %.3fms" screen1#lastLayoutTime);
        let time = Time.now() in
        progress1#setValue Float.(sin (time*.0.5) |> abs);
        data1();
        data2();
    );
;;

let _ = go()
