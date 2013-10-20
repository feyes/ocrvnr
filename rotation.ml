(* picture dimension *)
let get_dims pct =
  ((Sdlvideo.surface_info pct).Sdlvideo.w, (Sdlvideo.surface_info pct).Sdlvideo.h)

(* init SDl *)
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
      | _ -> wait_key()

(* show picture *)
let show pct dst = 
  let d = Sdlvideo.display_format pct in 
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

(* picture rotation *)
let iof a = int_of_float a 
let foi a = float_of_int a

let rotate pct =
  let (x, y) = get_dims pct in
  let (x_i, y_i) = (ref 0, ref 0) in
  let pi = 4. *. (atan 1.) in
  let theta = Detection.detection pct in
  let theta_f = (((float theta) *. pi) /. 180.) in
  let display = Sdlvideo.create_RGB_surface_format pct [] x y in
    for i = 0 to x - 1 do
      for j = 0 to y - 1 do 
        Sdlvideo.put_pixel_color display i j (255,255,255);
      done; 
    done ;

    for x_o = 0 to x-1 do
      for y_o = 0 to y-1 do
        begin
        (*print_int x_o;
        Printf.printf"\n";*)
 
        x_i := iof (foi x_o *. ((cos(theta_f))) -. foi  y_o *. ((sin(theta_f))));
        y_i := iof (foi x_o *. ((sin(theta_f))) +. foi y_o *. ((cos(theta_f))));
         
        (*print_int  !x_i;
         Printf.printf"\n";*)

        if (!x_i >= 0 && !y_i >= 0 && !x_i<x && !y_i<y) then
            Sdlvideo.put_pixel_color display x_o y_o (Sdlvideo.get_pixel_color pct !x_i !y_i);
        end
      done;
    done;
   display;;
