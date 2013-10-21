let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
 
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
  
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst
 
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()

let copy img dst =
  let (w,h) = get_dims img in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      Sdlvideo.put_pixel_color dst x y (Sdlvideo.get_pixel_color img x y);
    done;
  done

(*Principe: Run length smoothing algorithm, todo : XY-cut*)
let findText img =
  let (w,h) = get_dims img in
  for i = 0 to  h-1 do 
    let blackPx = ref 0 in
    let whitePx = ref 0 in
    let bound1 = ref (-1) in
    let bound2 = ref 0 in
    let distBpxWpx = ref (-1) in  
    let dist = ref 10 in
    let j = ref 0 in
    begin
      while !j < w do
	let (r,_,_) = (Sdlvideo.get_pixel_color img !j i) in
	if ( r = 255 ) then
	  begin
	    whitePx:= !whitePx + 1;
	    if(!distBpxWpx != -1) then
	      distBpxWpx := !distBpxWpx + 1;
	  end	  
	else
	  begin
	    distBpxWpx := 0;
	    if !bound1 = (-1) then
	      bound1 := !j;
	    blackPx := !blackPx + 1
	  end;
	if(!distBpxWpx > !dist) then
	  begin
	    bound2 := !j - !dist;
	    if (!bound1 != (-1)) then
	      for k = !bound1 to !bound2 do
		if (r != 0) then
		  Sdlvideo.put_pixel_color img k i (0,0,0)
	      done;
	    bound1 := -1
	  end;
	j := !j + 1
      done
    end
  done

 
let main () =
  begin
    if Array.length (Sys.argv) < 2 then
      failwith "Hé dit donc bonhomme, le nom du fichier il est où?";
    sdl_init ();
    let img = Sdlloader.load_image Sys.argv.(1) in
      let (w,h) = get_dims img in
	let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
	  show img display;
	  let dst = Sdlvideo.create_RGB_surface_format img [] w h in
	    copy img dst;
	    findText dst;
	    show dst display;
	    wait_key ();
	    exit 0
  end
    
let _ = main ()
