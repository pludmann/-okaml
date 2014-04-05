(*chargement*)
let int2 s =
  let i = String.index s ' ' in
  let a = int_of_string(String.sub s 0 i) and b = int_of_string (String.sub s (i+1) ((String.length s) - (i+1))) in
(a,b);;

let charge_fichier s =
  let f = open_in s in
  let (m,n) = int2 (input_line f) in
  let mat = Array.make_matrix m n false in
  for i = 0 to m-1 do
    let ligne = input_line f in
    for j = 0 to n-1 do
      mat.(i).(j) <- (ligne.[j] = '#')
    done
  done;
  close_in f;
(mat,m,n);;


let ecrit_fichier n l =
  let f = open_out "ans" in
  output_string f ((string_of_int n)^"\n");
  let rec aux l = match l with
    | [] -> ()
    | (instr,i,j,s)::t ->
      if instr = "p" then
	output_string f ("PAINTSQ "^(string_of_int i)^" "^(string_of_int j)^" "^(string_of_int s)^"\n")
      else
	output_string f ("ERASECELL "^(string_of_int i)^" "^(string_of_int j)^"\n")
	  ;
      aux t;
  in
  aux l;
();;


(* calcul *)
let pgc mat m n i j =
  let k = ref 0 in
  let fini = ref true in
  while (!fini) do
    for a = -(!k+1) to (!k+1) do
      for b = -(!k+1) to (!k+1) do
	if i+a < 0 or i+a >= m or j+b < 0 or j+b >= n then fini := false
	else (if not mat.(i+a).(j+b) then fini := false;)
      done;
    done;
    incr k;
  done;
(!k-1);;

let cherche_max v e m n =
  let max = ref 0 in
  let coo = ref (0,0) in
  for i = 0 to m-1 do
    for j = 0 to n-1 do
      if (not e.(i).(j)) && (v.(i).(j) >= !max) then (
	max := v.(i).(j);
	coo := (i,j);
      )
    done
  done;
!coo;;

let cherche_premier e m n =
  let i = ref 0 and j = ref 0 and coo = ref (0,0) and ans = ref true in
  while !ans && !i <= m-1 do
    while !ans && !j <= n-1 do
      if not e.(!i).(!j) then (coo := (!i,!j); ans := false;);
      incr j;
    done;
    j := 0;
    incr i;
  done;
!coo;;

let isfini e m n =
  let ans = ref true in
  let i = ref 0 and j = ref 0 in
  while (!ans) && !i <= m-1 do
    while (!ans) && !j <= n-1 do
      if not e.(!i).(!j) then ans := false;
      incr j;
    done;
    j := 0;
    incr i;
  done;
(!ans);;

let calcul mat m n =
  let instr = ref [] in
  let nbinstr = ref 0 in
  let voisins = Array.make_matrix m n 0 in
  for i = 0 to m-1 do
    for j = 0 to n-1 do
      voisins.(i).(j) <- pgc mat m n i j
    done
  done;
  let estok = Array.make_matrix m n false in
  let nbcasesapeindre = ref 0 in
  for i = 0 to m-1 do
    for j = 0 to n-1 do
      if not mat.(i).(j) then estok.(i).(j) <- true
      else incr nbcasesapeindre;
    done
  done;
  Printf.printf "Il y a %d cases à peindre.\n" (!nbcasesapeindre);
  let tmp = ref false in
  (try (
  while true do
    let (i,j) = (if not !tmp then cherche_max voisins estok m n else cherche_premier estok m n ;) in
    if (i,j) = (0,0) then raise Exit;
    let nb = voisins.(i).(j) in
    if nb = 0 then tmp := true;
(*    Printf.printf "case choisie : %d %d \n" i j; *)
    instr := ("p",i,j,nb)::(!instr);
    incr nbinstr;
    for k = i-nb to i+nb do
      for l = j-nb to j+nb do
	estok.(k).(l) <- true;
      done
    done;
  done;

    
    
  ) with _ -> ());
(!instr,!nbinstr);;





(*
let test = [| [| true; true; true; true; true|] ; [|true;true;true;true;true|] ; [|true;true;true;true;true|] ;  [|true;true;true;true;true|] ; [|true;true;true;true;true|] |];;
let () =
  print_int (pgc test 5 5 2 2)
*)

(* end *)

let () = 
  let (mat,m,n) = charge_fichier "doodle.txt" in
  let (l,nb) = calcul mat m n in
  ecrit_fichier nb l

