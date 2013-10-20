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

let iof a = int_of_float a
let foi a = float_of_int a

(* angle detection *)
let detection pct =
  let (x, y) = get_dims pct in
  let xf = foi(x) in
  let yf = foi(y) in 
  let p_max = iof(sqrt((xf *. xf) +. (yf *. yf))) in 
  let angle =  ref 0 in
  let cpt = ref 0 in
  let p_int = ref 0 in
  let p_float = ref 0. in
  let pi = 4. *. (atan 1.) in
  let matrix = Array.make_matrix (p_max) (91) (0) in
    for i=0 to x-1 do
      for j=0 to y-1 do
        if Sdlvideo.get_pixel_color pct i j = (0, 0, 0) then
          begin
          for theta = 0 to 90 do
            let theta_rad = (float theta)*. pi /. 180. in
            p_float := (float i)*. cos(theta_rad) +. (float j)*. sin(theta_rad);
            p_int := iof(!p_float);
            matrix.(!p_int).(theta) <- matrix.(!p_int).(theta) +1;
            if (matrix.(!p_int).(theta) > !cpt)  then 
                begin
                    angle := theta;
                    cpt := matrix.(!p_int).(theta);
                end
          done;
          end
      done;
    done;
    !angle -90;;

(*let main () =
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
    
  let angle = detection pct in
    Printf.printf "%d" angle;
    exit 0
  end 

let _ = main () *)
