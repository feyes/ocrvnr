type neuron =
    {
   mutable active:bool;
   mutable x:int;
   mutable w:float;
    }
let iof a =
  int_of_float a
let foi a =
  float_of_int a
    let a = 
      {active = false;x = Random.int 2;w = 0.12}
    let b =
      {active = false; x= Random.int 2;w=0.32}
    let h =
      {active = false;x = 1;w = 0.}
    let activation e1 =
      let rec somme = function
	|[] -> 0.
	| e::l -> foi(e.x) *. e.w +. somme l
      in
      somme e1
    let fonction_exit result = 
      1./.(1.+.exp(result*.(-1.)));;
Random.int 2
