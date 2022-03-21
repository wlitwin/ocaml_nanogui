type t = {
    mutable last_events : int;
    mutable events : int;
    mutable last_report_time : float;
    report_fn : events:int -> avg:float -> unit;
    report_sec : float;
}

let create ~report_fn ~report_sec = {
    last_events = 0;
    events = 0;
    last_report_time = Backend.Time.now();
    report_fn;
    report_sec;
}

let mark_event t =
    t.events <- t.events + 1;
    let time = Backend.Time.now() in
    let diff = time -. t.last_report_time in
    if Float.(time -. t.last_report_time >= 1.) then (
        t.last_events <- t.events;
        let avg_time = diff /. Float.of_int t.events *. 1000. in
        t.report_fn ~events:t.events ~avg:avg_time;
        t.events <- 0;
        t.last_report_time <- time;
    )
;;
