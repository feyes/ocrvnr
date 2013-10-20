(* picture dimension *)
let get_dims pct =
  ((Sdlvideo.surface_info pct).Sdlvideo.w, (Sdlvideo.surface_info pct).Sdlvideo.h)

(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

(* wait a key *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()

(* show picture *)
let show pct dst = 
  let d = Sdlvideo.display_format pct in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

let main () =
  begin
  (* want 1 argument *)
  if Array.length (Sys.argv) < 2 then 
    failwith "Any file";
  (* sdl init *)
  sdl_init ();
  (* load picture *)
  let pct = Sdlloader.load_image Sys.argv.(1) in
  (* picture dimension *)
  let (w,h) = get_dims pct in
  let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    show pct display;
    wait_key ();

  let angle = Detection.detection pct in
    Printf.printf "%d" angle;
    wait_key ();
    
    let display2 = Rotation.rotate pct in
      show display2 display;
      wait_key ();
     
    exit 0
  end 

let _ = main ()
