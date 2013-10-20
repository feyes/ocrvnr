(*init a 0 de Pentree et b*)
let entriesW =
    ref (Array.make 2 (-0.15));;

!entriesW.(0) <- 0.05

let entriesX =
    [|0;1;0;1;1|]

let b = ref 0.

let a = ref 0.3

let t = 10000

let vectorlearn =
    let vectorLearn = Array.make 3 [|1|] in
    vectorLearn.(0) <- [|0;1|];
    vectorLearn.(1) <- [|1;0|];
    (*vectorLearn.(2) <- [|1|];*)
    vectorLearn

let fs entriesX entriesW =
    let r = ref 0. in
    r := 0.;
    for i = 0 to (*entries.length*)2 -1 do
        for j = 0 to 1 do
            r := !r +. float_of_int (entriesX.(i).(j)) *. entriesW.(j)
        done;
    done;
    r := !r +. !b;
    !r

let learningLoop =
    let y =
        ref (fs vectorlearn !entriesW)
    in
    for i = 0 to t do      
        (*let y =
            ref (fs vectorlearn)
        in*)
        y := fs vectorlearn !entriesW;
Printf.printf "\n%f " !y;

        if !y >= 0.5 then
            y := 1.;
        if !y < 0.5 then
            y := 0.;
            Printf.printf "%f " !y;
            if !y <> 1. then
                begin
 Printf.printf "lol";

                for j = 0 to (*entriesW.length*) 2 -1 do
                    !entriesW.(j) <- !entriesW.(j) +. (*vectorlearn.(0) *.*) !a;
                    Printf.printf "%f" !entriesW.(j);
                    Printf.printf "coucou\n"
                done;
                b := !b +. !a
                end
    done

    let _ =
        learningLoop
(*let learning t =
    for i = 0 to t do
        let y fs vector = fs vector in 
        y fs vectorlearn;
        (*begin
        if  (y >= 0.) then
            y = 1.
        else
            y = 0.
            end*)
            if (y <> t.(i)) then
                begin
                    for j = 0 to entriesW.length -1 do
                        entriesW.(j) := !entriesW.(j) +. float_of_int
                        (entriesX.(j)) *. !a *. g;
                    done;
                    b := !b +. !a *. g
                end
    done*)
