include Ocaml_nanogui.Application

module Scheduler = Async_kernel.Async_kernel_scheduler.Private

open Base

let run ?(jobs_per_loop=500) ?(idle=fun()->()) screens =
    let open Async_kernel.Async_kernel_scheduler in
    set_max_num_jobs_per_priority_per_cycle jobs_per_loop;
    run ~idle:(fun () ->
        let t = Scheduler.t() in
        begin match Scheduler.uncaught_exn t with
        | None -> ()
        | Some e -> Error.to_string_hum e |> Caml.print_endline
        end;
        Scheduler.run_cycle t;
        idle()
    ) screens
