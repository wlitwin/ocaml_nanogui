type item = <
    id : string;
    preferredSize : Widget.graphics_context -> Vec2.t;
    setSize : Vec2.t -> unit;
    setPosition : Vec2.t -> unit;
    performLayout : Widget.graphics_context -> unit;
>

type dep_field = DLeft
               | DRight
               | DTop
               | DBottom

type item_dep = item * dep_field

module PolyHash = Hashtbl

type field_tbl = (item_dep, float) PolyHash.t

let concat_arrays arr =
    let sum = Array.fold_left (fun s a -> s + Array.length a) 0 arr in
    if sum = 0 then [||]
    else begin
        let init =
            let rec loop i =
                if i >= (*sum then failwith "Impossible"*) Array.length arr then failwith "Impossible"
                else begin
                    if not (Array.length arr.(i) = 0) then (
                        arr.(i).(0)
                    ) else loop (i+1)
                end
            in
            loop 0
        in
        let out = Array.make sum init in
        let total_arrays = Array.length arr in
        let idx = ref 0 in
        for i = 0 to total_arrays-1 do
            let src = arr.(i) in
            let len = Array.length src in
            if len > 0 then (
                Array.blit src 0 out (!idx) len;
                idx := len + !idx
            )
        done;
        out
    end
;;

let%expect_test _ =
    let p = Array.iter (fun i -> Printf.printf "%d " i) in
    p (concat_arrays [| [|1;2;3|]; [|4;5;6|] |]);
;;

module Constraint = struct
    type t = (* Relative to the window/layout rect top/bot/etc *)
           | WTop
           | WBottom
           | WLeft
           | WRight
           (* Relative to an item *)
           | ITop of item
           | IBottom of item
           | ILeft of item
           | IRight of item
           (* Combinations *)
           | Add of t array
           | Sub of t array
           | Mul of t array
           | Div of t array
           | Max of t array
           | Min of t array
           | FunDep of item_dep array * (field_tbl -> Widget.graphics_context -> float)
           | Const of float

    let rec gather_dependencies = function
        | ITop item -> [|item, DTop|]
        | IBottom item -> [|item, DBottom|]
        | ILeft item -> [|item, DLeft|]
        | IRight item -> [|item, DRight|]
        | Add lst
        | Sub lst
        | Mul lst
        | Div lst
        | Max lst
        | Min lst -> lst |> Array.map gather_dependencies |> concat_arrays
        | FunDep (lst, _) -> lst
        | WTop
        | WLeft
        | WRight
        | WBottom
        | Const _ -> [||]
;;

    let fun_dep deps fn = FunDep (deps, fn)

    (* Lots of common helper methods *) 
    let relative t amt = Add [|t; Const amt|]

    let preferredW item = 
        let key1 = (item :> item), DLeft in
        (*let key2 = (item :> item), DRight in*)
        FunDep ([|key1; (*key2*)|], fun tbl ctx -> 
            PolyHash.find tbl key1 +. ((item :> item)#preferredSize ctx).a
    )

    let prefW_no_dep item =
        FunDep ([||], fun _tbl ctx -> ((item :> item)#preferredSize ctx).a)

    let prefH_no_dep item =
        FunDep ([||], fun _tbl ctx -> ((item :> item)#preferredSize ctx).b)

    let preferredH item = 
        let key1 = (item :> item), DTop in
        (*let key2 = (item :> item), DBottom in*)
        FunDep ([|key1; (*key2*)|], fun tbl ctx -> 
            PolyHash.find tbl key1 +. ((item :> item)#preferredSize ctx).b
    )

    let wTop = relative WTop
    let wLeft = relative WLeft
    let wRight = relative WRight
    let wBottom = relative WBottom

    let topOf item = relative (ITop (item :> item))
    let leftOf item = relative (ILeft (item :> item))
    let rightOf item = relative (IRight (item :> item))
    let bottomOf item = relative (IBottom (item :> item))
    let widthOf item = Sub [|IRight (item :> item); ILeft (item :> item)|]
    let heightOf item = Sub [|IBottom (item :> item); ITop (item :> item)|]

    let mul a b = Mul [|a; b|]
    let add a b = Add [|a; b|]
    let sub a b = Sub [|a; b|]
    let div a b = Div [|a; b|]
    let max a b = Max [|a; b|]
    let const c = Const c

    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mul
    let ( / ) = div

    let centerTop toCenter ref = 
        let ref = (ref :> item) in
        FunDep ([|(ref, DTop); (ref, DBottom)|], fun fields ctx ->
            let lookup f = Hashtbl.find fields f in
            let top = lookup (ref, DTop)
            and bot = lookup (ref, DBottom) in
            let prefH = (toCenter#preferredSize ctx).Vec2.b in
            top +. ((bot -. top) -. prefH)*.0.5
    )
end

module Bounds = struct
    type t = {
        top : Constraint.t;
        left : Constraint.t;
        right : Constraint.t;
        bottom : Constraint.t;
    }
end

(*
type constraint_inputs = {
    input_item : Widget.layoutable;
    input_loc : Bounds.t;
}
*)

module ItemConstraints = struct
    type t = {
        item : item;
        loc : Bounds.t;
    }

    module Build = struct
        type ic = t
        include Constraint

        type constraint_ = t
        type t = ic

        let lift item = {
            item = (item :> item);
            loc = Bounds.{
                top = WTop;
                left = WLeft;
                bottom = WBottom;
                right = WRight;
            };
        }

        let item t = t.item
        let loc t = t.loc

        let cast item = (item :> item)

        let top_left x y t = {
            t with loc = {
                t.loc with
                left = x;
                top = y;
            }
        }

        let top_right x y t = {
            t with loc = {
                t.loc with
                right = x;
                top = y;
            }
        }

        let bottom_left x y t = {
            t with loc = {
                t.loc with
                left = x;
                bottom = y;
            }
        }

        let bottom_right x y t = {
            t with loc = {
                t.loc with
                right = x;
                bottom = y;
            }
        }

        let tl_size x y w h t = {
            t with loc = {
                left = x;
                top = y;
                right = (leftOf t.item 0. + w);
                bottom = (topOf t.item 0. + h);
            }
        }

        let tr_size x y w h t = {
            t with loc = {
                right = x;
                top = y;
                left = (rightOf t.item 0. - w);
                bottom = (topOf t.item 0. + h);
            }
        }

        let tl_pref_size x y t = {
            t with loc = {
                left = x;
                top = y;
                right = leftOf t.item 0. + prefW_no_dep t.item;
                bottom = topOf t.item 0. + prefH_no_dep t.item;
            }
        }

        let tr_pref_size x y t = {
            t with loc = {
                right = x;
                top = y;
                left = rightOf t.item 0. - prefW_no_dep t.item;
                bottom = topOf t.item 0. + prefH_no_dep t.item;
            }
        }

        let bl_pref_size x y t = {
            t with loc = {
                left = x;
                bottom = y;
                right = leftOf t.item 0. + prefW_no_dep t.item;
                top = bottomOf t.item 0. - prefH_no_dep t.item;
            }
        }

        let br_pref_size x y t = {
            t with loc = {
                right = x;
                bottom = y;
                left = rightOf t.item 0. - prefW_no_dep t.item;
                top = bottomOf t.item 0. - prefH_no_dep t.item;
            }
        }

        let left v t = { t with loc = { t.loc with left = v }}
        let right v t = { t with loc = { t.loc with right = v }}
        let top v t = { t with loc = { t.loc with top = v }}
        let bottom v t = { t with loc = { t.loc with bottom = v }}
    end
end

module Build = ItemConstraints.Build

module DependencyGraph = struct
    type t = {
        constraints : (item, ItemConstraints.t) PolyHash.t;
        top_sort : (item * dep_field) array;
    }

    type field_tuple = item * Constraint.t * dep_field
    type visited_set = (item_dep, unit) PolyHash.t

    exception Not_a_DAG of dep_field
    (* Topological sort, based on depth-first search *)
    let depth_sort (tbl : (item_dep, item_dep array) PolyHash.t) (all : field_tuple list) : item_dep array =
        let sorted = ref [] in
        let temp = PolyHash.create 10 in
        let perm = PolyHash.create 10 in
        let has_temp i = PolyHash.mem temp i in
        let has_perm i = PolyHash.mem perm i in
        let mark_temp i = PolyHash.replace temp i () in
        let mark_perm i = PolyHash.replace perm i () in
        let rec visit (_item, dep as key : item_dep) =
            if has_perm key then ()
            else if has_temp key then raise (Not_a_DAG dep)
            else begin
                mark_temp key;
                let lst = PolyHash.find tbl key in
                Array.iter visit lst;
                mark_perm key;
                sorted := key :: !sorted;
            end
        in
        List.iter (fun (i, _, d) -> visit (i, d)) all;
        List.rev !sorted |> Array.of_list
    ;;

    let calculate_dependency_graph (lst : ItemConstraints.t list) : t =
        let open ItemConstraints in
        let constraint_table = PolyHash.create 10 in
        List.iter (fun c -> 
            PolyHash.replace constraint_table c.item c;
        ) lst;
        let lst = List.fold_left (fun accum c ->
            (c.item, c.loc.top, DTop)
            :: (c.item, c.loc.bottom, DBottom)
            :: (c.item, c.loc.left, DLeft)
            :: (c.item, c.loc.right, DRight)
            :: accum
        ) [] lst in
        let tbl = PolyHash.create 10 in
        List.iter (fun (item, loc, depField) ->
            let key = (item, depField) in
            let dep_lst = Constraint.gather_dependencies loc in
            match PolyHash.find_opt tbl key with
            | Some lst -> PolyHash.replace tbl key (Array.append dep_lst lst)
            | None -> PolyHash.replace tbl key dep_lst 
        ) lst;
        {
            constraints = constraint_table;
            top_sort = depth_sort tbl lst;
        }
    ;;
end

let sel_field tbl (item, field) =
    let open ItemConstraints in
    let c = PolyHash.find tbl item in
    match field with
    | DTop -> c.loc.top
    | DBottom -> c.loc.bottom
    | DLeft -> c.loc.left
    | DRight -> c.loc.right
;;

let rec calc_loc (ctx : Widget.graphics_context) (screen : Rect.t) field_tbl loc =
    let lookup t = PolyHash.find field_tbl t in
    let recur = calc_loc ctx screen field_tbl in
    let app (init, f, arr) : float =
        arr 
        |> Array.map recur
        |> Array.fold_left f init
    in
    let dual_map (f, arr) : float =
        let res = ref (recur arr.(0)) in
        let len = Array.length arr in
        for i=1 to len-1 do
            res := f !res (recur arr.(i))
        done;
        !res
    in
    let open Constraint in
    match loc with
    | WTop -> screen.y
    | WLeft -> screen.x
    | WRight -> screen.x +. screen.w
    | WBottom -> screen.y +. screen.h
    | ITop item -> lookup (item, DTop)
    | ILeft item -> lookup (item, DLeft)
    | IRight item -> lookup (item, DRight)
    | IBottom item -> lookup (item, DBottom)
    (* Hacky - dynamically recalculate the size *)
    | Max lst -> app (Float.min_float, Float.max, lst)
    | Min lst -> app (Float.max_float, Float.min, lst)
    | Add lst -> app (0., Float.add, lst)
    | Mul lst -> app (1., Float.( * ), lst)
    | Sub lst -> dual_map (Float.sub, lst)
    | Div lst -> dual_map (Float.(/), lst)
    | FunDep (_, f) -> f field_tbl ctx
    | Const c -> c
;;

let layout_single_item ctx screen deps field_tbl (key : item_dep) =
    let loc = sel_field deps key in
    let res = calc_loc ctx screen field_tbl loc in
    PolyHash.replace field_tbl key res;
;;

let set_rectangle tbl item =
    let lookup field = PolyHash.find tbl (item, field) in
    let rect = Aabb.{
        x1 = lookup DLeft;
        y1 = lookup DTop;
        x2 = lookup DRight;
        y2 = lookup DBottom;
    } |> RectAabb.rect_of_aabb in
    item#setPosition Vec2.(mk rect.x rect.y);
    item#setSize Vec2.(mk rect.w rect.h)
;;

let layout ctx rect deps =
    let open DependencyGraph in
    let field_tbl = PolyHash.create 10 in
    Array.iter (layout_single_item ctx rect deps.constraints field_tbl) deps.top_sort;
    PolyHash.iter (fun k _ -> 
        set_rectangle field_tbl k;
        k#performLayout ctx;
    ) deps.constraints
;;

open Widget

class constraintLayout (rules : ItemConstraints.t list) =
    let deps = DependencyGraph.calculate_dependency_graph rules in
    object(_self)
        method preferredSize (_ctx : graphics_context) (_widget : widget) =
            (* Try to calculate minimum size? *)
            Vec2.(mk 100. 100.)
            (* widget#preferredSize ctx *) (* Can't ask the widget.. it uses us... DUH *)

        method performLayout (ctx : graphics_context) (widget : widget) =
            let containerSize = match widget#fixedSize with
                     | None -> widget#size
                     | Some fs -> fs
            in
            let rect = Rect.{x=0.; y=0.; w=containerSize.a; h=containerSize.b} in
            layout ctx rect deps
    end

