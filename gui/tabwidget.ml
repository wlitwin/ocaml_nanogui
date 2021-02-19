open! Widget
open! Stackedwidget
open! Button
open ConstraintLayout
open Scrollpanel
open BoxLayout
open Pushbutton
open Buttongroup

class tabwidget parent = object(self)
    inherit widget parent as super

    val header = new widget None
    val scroller = new scrollpanel None
    val scrollContainer = new widget None
    val content = new stackedwidget None
    val buttonGroup = new buttongroup

    initializer
        super#addChild self#childCount (header :> widget);
        super#addChild self#childCount (content :> widget);
        buttonGroup#setAlwaysOneSelection true;

        (* Header is:
         *
         *    H Scroller
         * L |[tab1] [tab2] [tab3]..| R
         * ----------------------------
         *)
        let h_deps = Build.[
            lift scroller
            (*|> bottom (FunDep ([||], (fun _ ctx -> (scrollContainer#preferredSize ctx).b)))*)
            ;
        ] in

        scroller#addChildWidget (scrollContainer :> widget);
        scroller#setId "tabwidget_scroller";
        scrollContainer#setLayout (new boxLayout Horizontal Middle 0. 0. :> layout);
        scrollContainer#setId "tabwidget_scrollcontainer";
        scroller#setOverflowY false;
        header#setLayout (new constraintLayout h_deps);
        header#addChildWidget (scroller :> widget);

        let pad = 4. in
        let deps = Build.[
            lift header
            |> left (wLeft pad)
            |> right (wRight ~-.pad)
            |> bottom (max 
                        (fun_dep [||] (fun _ ctx ->
                            match scrollContainer#childAt 0 with
                            | None -> 50.
                            | Some w -> (w#preferredSize ctx).b +. (if scroller#hasOverflowX then scroller#scrollBarSize else 0.) +. 2.
                        ))
                        (wBottom 0. * const 0.1)
                      );
            
            lift content
            |> top (bottomOf header 0.)
            |> left (wLeft pad)
            |> right (wRight ~-.pad)
            |> bottom (wBottom ~-.pad)
        ] in

        self#setLayout (new constraintLayout deps)

    method! addChild _ = failwith "Tab widget use createTab"
    method! addChildWidget _ = failwith "Tab widget use createTab"

    method private addTabPriv index label tab =
        assert (index <= self#tabCount);
        content#addChild index tab;
        let btn = new pushbutton (Some (scrollContainer :> widget)) label in
        btn#setChangeCallback (fun down ->
            if down then (
                self#setActiveTab index
            );
        );
        buttonGroup#addItem (btn :> togglable);
        self#setActiveTab index

    method setActiveTab index =
        assert (index < self#tabCount);
        buttonGroup#setItem index;
        content#setSelectedIndex index;

    method tabCount =
        content#childCount

    method createTabIndex index label =
        let tab = new widget None in
        self#addTabPriv index label tab;
        tab

    method createTab (label : string) =
        self#createTabIndex self#tabCount label

    method! draw ctx =

        super#draw ctx;
        let open Nanovg in

        let open Float in
        let pos = content#position in
        let size = content#size in
        let corner = theme#buttonCornerRadius in
        begin_path ctx;
        stroke_width ctx 1.;
        rounded_rect ctx (pos.a-1.5) (pos.b-1.5) (size.a+1.) (size.b+1.) corner;
        stroke_color ctx theme#borderLight;
        stroke ctx;

        rounded_rect ctx (pos.a-0.5) (pos.b-0.5) (size.a+1.5) (size.b+1.5) corner;
        stroke_color ctx theme#borderDark;
        stroke ctx;
end
