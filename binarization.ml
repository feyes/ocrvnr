(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info
					      img).Sdlvideo.h)
    
(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
    
(* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
  match e with
    Sdlevent.KEYDOWN _ -> ()
  | _ -> wait_key ()
    
(*
 *   show img dst
 *     affiche la surface img sur la surface de destination
 *     dst (normalement l''ecran)
 *     *)
let show img dst =
  let d = Sdlvideo.display_format img in
  Sdlvideo.blit_surface d dst ();
  Sdlvideo.flip dst
    
(*Mes fonctions*)
let level (r,g,b) =
  0.3 *. float_of_int r +. 0.59 *. float_of_int g +. 0.11 *. float_of_int b 
    
let color2grey (r,g,b) =
  (int_of_float(level(r,g,b)), int_of_float(level(r,g,b)), int_of_float(level(r,g,b)) )
    
let image2grey src dst =
  let (w,h) =     get_dims src in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      Sdlvideo.put_pixel_color dst i j(
        color2grey(Sdlvideo.get_pixel_color src i j))
    done
  done
    
let grey2BW (r,g,b) =
  if((r+g+b)/3 >160) then
    (255,255,255)
  else
    (0,0,0)
      
let image2BW src dst =
  let (w,h) =
    get_dims src in
  for j = 0 to h-1 do
    for i = 0 to w-1 do
      Sdlvideo.put_pixel_color dst i j (grey2BW(Sdlvideo.get_pixel_color
						  src i j))
    done
  done
    
let onecolor (r,g,b) =
  r
    
let areacolors src i j w h =
  let total = ref 0 in
  total := 0;
  for x = i -1 to i + 1 do
    for y = j -1 to j+1 do
      total := !total + onecolor (Sdlvideo.get_pixel_color src x y)
    done;
  done;
  !total
    
let oneiswhite src x y i j =
  let r = ref false in
  for k = i -1 to i + 1 do
    for l = j -1 to j + 1 do
      if i >= 0 && i < x && j >=0 && j<y then
	if ((Sdlvideo.get_pixel_color src k l) = (255,255,255)) then
	  r := true
    done;
  done;
  !r
    (*
let erode src dst x y =
        let i = ref 3 in
        let j = ref 3 in
  while !i < x -4 do
    while !j < y -4 do
            Sdlvideo.put_pixel_color dst !i !j (255,255,255);
incr i;
incr j
    done
  done *)
let backNoise src dst =
  let (w,h) = 
    get_dims src in
  for i = 2 to w-3 do
    for j = 2 to h-3 do
      if areacolors src i j w h <= 255 *8  then
	Sdlvideo.put_pixel_color dst i
          j(0,0,0)
      else
        Sdlvideo.put_pixel_color dst i j
          (255,255,255)
    done
  done
    
(* main *)
let main () =
  begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On r'ecup`ere les dimensions *)
    let (w,h) = get_dims img in
    (* On cr'ee la surface d'affichage en
		     * doublebuffering *)
    let display =
      Sdlvideo.set_video_mode w h
        [`DOUBLEBUF] in
    (* on affiche l'image *)
    show img display;
    (* on attend une touche
     * *)
    wait_key ();
    
    let dest = 
      Sdlvideo.create_RGB_surface_format img [] w h in
    
    image2grey img dest;
    
    show dest display;
    
    wait_key ();
    
    let final =
      Sdlvideo.create_RGB_surface_format dest [] w h in
    image2BW dest final;
    
    show final display;
    
    wait_key();
    
    let nonoise =
      Sdlvideo.create_RGB_surface_format final [] w h in
        backNoise final nonoise;
    (*erode final nonoise w h;*)
    show nonoise display;
    
    wait_key();
    
    Sdlvideo.save_BMP nonoise "binarized.bmp";
    
     (* on quitte
      * *)
    exit 0
  end
    
let _ = main ()
  
    
