let b =
    ref 0.

let a =
    Random.float 1.

let t =
    1000

let verification entryTab pw =
    let r = ref !b
    in
    r := !b;
    for i = 0 to Array.length entryTab -1 do
        r := !r +. float_of_int(entryTab.(i)) *. pw.(i)
    done;
    !r

let isintab reference tab =
    let i = ref 0 in
    i := 0;
    while !i < Array.length reference -1 &&
    (tab.(0),tab.(1)) <> reference.(!i) do
        i := !i +1
    done;
    (tab.(0),tab.(1)) = reference.(!i)

let learning reference pw xn =
    let y =
        ref (verification xn pw)
    in
    for i = 0 to t do
        y := verification xn pw;
        Printf.printf "fs=%f  " !y;
        Printf.printf "x1=%d " ((xn.(0)));
        Printf.printf "x2=%d " ((xn.(1)));
        if !y >= 0. then
            y := 1.
        else
            begin
                y := 0.
            end;
        Printf.printf " Q=%d  \n" (int_of_float(!y));
        if (isintab reference xn) && !y <> 1. then
            begin
                for j = 0 to (Array.length pw) -1 do
                pw.(j) <- pw.(j) +. float_of_int (xn.(j)) *. a
                done;
                b := !b +. a
            end
        else
            if (not(isintab reference xn)) && !y <> 0. then
                begin
                    for j = 0 to (Array.length pw) -1 do
                    pw.(j) <- pw.(j) -. float_of_int(xn.(j)) *. a
                    done;
                    b := !b -. a
                end
    done;
int_of_float(!y)

let complement x =
    if x > 0 then
        0
    else
        1

let pw1 =
    [|(Random.float 2.) -.1.; (Random.float 2.) -.1.|]
let pw2 =
    [|(Random.float 2.) -.1.; (Random.float 2.) -.1.|]
let pw3 =
    [|(Random.float 2.) -.1.; (Random.float 2.) -.1.|]


let perceptron x1 x2 =
    let nx1 = complement x1 in
    let nx2 = complement x2 in
    let a =
        learning [|(1,1)|] pw1 [|nx1;x2|]
    in
    let b =
        learning [|(1,1)|] pw2 [|x1;nx2|]
    in
    learning [|(1,0);(0,1)|] pw3 [|a;b|]

let _ =
    if Array.length (Sys.argv) < 2 then
        failwith "Veuillez entrer deux arguments binaire.";
    Printf.printf "%d\n" (perceptron (int_of_string(Sys.argv.(1)))
        (int_of_string(Sys.argv.(2))))
