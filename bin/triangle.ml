(*
   Minimal Tgles2 example. This code is in the public domain.
   Draws a fantastic tri-colored triangle.

   Compile with:
   ocamlfind ocamlc -linkpkg -package result,tsdl,tgls.tgles2 -o trigles2.byte \
                    trigles2.ml
   ocamlfind ocamlopt -linkpkg -package result,tsdl,tgls.tgles2 -o trigles2.native \
                      trigles2.ml
*)

open Tgles2
open Result

let str = Printf.sprintf

let ( >>= ) x f = match x with Ok v -> f v | Error _ as e -> e

(* Helper functions. *)

let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

let get_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f -> f a; Int32.to_int a.{0}

let set_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f i -> a.{0} <- Int32.of_int i; f a

let get_string len f =
  let a = bigarray_create Bigarray.char len in
  f a; Gl.string_of_bigarray a

(* Shaders *)

let vertex_shader = "
  #version 100
  attribute vec3 vertex;
  attribute vec3 color;
  uniform float time;
  varying vec4 v_color;
  void main()
  {
    v_color = vec4(color, 1.0);
	float scale = sin(time*2.) * 0.25 + 0.75;
    gl_Position = vec4(vertex*scale, 1.0);
  }"

let fragment_shader = "
  #version 100
  precision highp float;
  uniform float time;
  varying vec4 v_color;
  void main() { gl_FragColor = v_color; }"

(* Geometry *)

let set_3d ba i x y z =
  let start = i * 3 in
  ba.{start} <- x; ba.{start + 1} <- y; ba.{start + 2} <- z

let vertices =
  let vs = bigarray_create Bigarray.float32 (3 * 3) in
  set_3d vs 0 (-0.8) (-0.8) 0.0;
  set_3d vs 1 0.8    (-0.8) 0.0;
  set_3d vs 2 0.0    0.8    0.0;
  vs

let colors =
  let cs = bigarray_create Bigarray.float32 (3 * 3) in
  set_3d cs 0 1.0 0.0 0.0;
  set_3d cs 1 0.0 1.0 0.0;
  set_3d cs 2 0.0 0.0 1.0;
  cs

let indices =
  let is = bigarray_create Bigarray.int8_unsigned 3 in
  set_3d is 0 0 1 2;
  is

(* OpenGL setup *)

let create_buffer b =
  let id = get_int (Gl.gen_buffers 1) in
  let bytes = Gl.bigarray_byte_size b in
  Gl.bind_buffer Gl.array_buffer id;
  Gl.buffer_data Gl.array_buffer bytes (Some b) Gl.static_draw;
  id

let delete_buffer bid =
  set_int (Gl.delete_buffers 1) bid

let create_geometry () =
  let iid = create_buffer indices in
  let vid = create_buffer vertices in
  let cid = create_buffer colors in
  Ok (iid, vid, cid)

let delete_geometry (iid, vid, cid) =
  delete_buffer iid; delete_buffer vid; delete_buffer cid;
  Ok ()

let compile_shader src typ =
  let get_shader sid e = get_int (Gl.get_shaderiv sid e) in
  let sid = Gl.create_shader typ in
  Gl.shader_source sid src;
  Gl.compile_shader sid;
  if get_shader sid Gl.compile_status = Gl.true_ then Ok sid else
  let len = get_shader sid Gl.info_log_length in
  let log = get_string len (Gl.get_shader_info_log sid len None) in
  (Gl.delete_shader sid; Error (`Msg log))

let create_program () =
  compile_shader vertex_shader Gl.vertex_shader >>= fun vid ->
  compile_shader fragment_shader Gl.fragment_shader >>= fun fid ->
  let pid = Gl.create_program () in
  let get_program pid e = get_int (Gl.get_programiv pid e) in
  Gl.attach_shader pid vid; Gl.delete_shader vid;
  Gl.attach_shader pid fid; Gl.delete_shader fid;
  Gl.bind_attrib_location pid 0 "vertex";
  Gl.bind_attrib_location pid 1 "color";
  Gl.link_program pid;
  if get_program pid Gl.link_status = Gl.true_ then Ok pid else
  let len = get_program pid Gl.info_log_length in
  let log = get_string len (Gl.get_program_info_log pid len None) in
  (Gl.delete_program pid; Error (`Msg log))

let delete_program pid =
  Gl.delete_program pid; Ok ()

let last_time = ref 0.
let draw pid (iid, vid, cid) uid offset =
  let bind_attrib id loc dim typ =
    Gl.bind_buffer Gl.array_buffer id;
    Gl.enable_vertex_attrib_array loc;
    Gl.vertex_attrib_pointer loc dim typ false 0 (`Offset 0);
  in
  (*
  Gl.clear_color 0. 0. 0. 1.;
  Gl.clear Gl.color_buffer_bit;
  *)
  Gl.use_program pid;
  Gl.bind_buffer Gl.element_array_buffer iid;
  Gl.uniform1f uid GLFW.(getTime() +. offset);
  bind_attrib vid 0 3 Gl.float;
  bind_attrib cid 1 3 Gl.float;
  Gl.draw_elements Gl.triangles 3 Gl.unsigned_byte (`Offset 0);
  Gl.bind_buffer Gl.array_buffer 0;
  Gl.bind_buffer Gl.element_array_buffer 0;
;;

