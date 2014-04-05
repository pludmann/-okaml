open Printf;;

(*chargement*)
let lit_int s =
  try (
    let i = String.index s ' ' in
    let a = int_of_string(String.sub s 0 i) and s' = String.sub s (i+1) (String.length s - (i+1)) in
    (a,s')
  ) with _ -> (int_of_string s, "")
;;
 
let charge_fichier s =
  let f = open_in s in
  let s = input_line f in
  let (inter,s) = lit_int s in
  let (rues,s) = lit_int s in
  let (temps,s) = lit_int s in
  let (nbcar,s) = lit_int s in
  let (start,s) = lit_int s in
  for i = 1 to inter do
    ignore(input_line f);
  done;
  let liste = ref [] in
  for i = 1 to rues do
    let s = input_line f in
    let (a,s) = lit_int s in
    let (b,s) = lit_int s in
    let (sens,s) = lit_int s in
    let (cout,s) = lit_int s in
    let (long,s) = lit_int s in
    liste := (a,b,sens,cout,long)::(!liste);
  done;
(inter,rues,temps,nbcar,start,!liste);;

let tableau_adj l inter =
  let v = Array.make inter [] in
  let rec aux l = match l with
    | [] -> ()
    | (a,b,s,c,l)::t -> 
      v.(a) <- (b,c,l)::(v.(a));
      if s = 2 then v.(b) <- (a,c,l)::(v.(b));
      aux t;
  in
  aux l;
v;;

(**** OUTPUT ****)
let ecrit_fichier trajets =
  let n = Array.length trajets in
  let f = open_out "ans_math" in
  output_string f ((string_of_int n)^"\n");
  let rec out l = match l with
    | [] -> ()
    | h::t -> output_string f ((string_of_int h)^"\n"); out t;
  in
  for i = 0 to n-1 do
    output_string f ((string_of_int (List.length trajets.(i)))^"\n");
    out trajets.(i);
  done;
();;
    



(**** calcul ****)
let e1 (a,b,c) = a;;
let e2 (a,b,c) = b;;
let e3 (a,b,c) = c;;


let rec print_liste l = match l with 
  | [] -> printf "\n"
  | [h] -> printf "%d\n" h
  | h::t -> printf "%d -> " h; print_liste t;
;;

let numerorue a b liste = 
  let rec aux liste n = match liste with
    | [] -> failwith "numero rue not found"
    | (x,y,s,_,_)::t -> if (x,y) = (a,b) || ( (y,x) = (a,b) && s = 2 ) then n else aux t (n+1)
  in
  aux liste 0;;

let calcul_chemin pos voisin visite t c liste =
  let rec parcours pos tps k chemin longueur =
(*    printf "parcours %d %d %d %d" pos tps k longueur; print_liste chemin; *)
    if k = 0 then (chemin, longueur, tps)
    else (
      let chemax = ref (chemin, longueur, tps) in
(*      printf "début aux %d\n" (List.length voisin.(pos)); *)
      let rec aux voisins = match voisins with
	| [] -> ()
	| (b,c,l)::t -> 
	  if tps - c > 0 then 
	    let (che,lon,tem) = parcours b (tps-c) (k-1) (b::chemin) (longueur+(if visite.(numerorue pos b liste) then 0 else l)) in
	    if lon > e2 (!chemax) then chemax := (che,lon,tem);
	  else ();
	  aux t;
      in
      aux (voisin.(pos));
(*      printf "fin aux\n"; *)
      !chemax; 
    )
  in
  parcours pos t c [] 0
;;

let random_chemin c t pos voisins =
  let rec aux chemin pos tps k = 
    if k = 0 then (chemin, pos, tps)
    else (
      let nbchoix = List.length (voisins.(pos)) in 
      let choix = Random.int nbchoix in
      let (b,c,l) = List.nth (voisins.(pos)) choix in
      aux (b::chemin) b (tps - c) (k-1)
    )
  in
  aux [] pos t c
;;

  

let rec marque chemin pos visites liste = match chemin with
  | [] -> ()
  | b::t -> visites.(numerorue pos b liste) <- true; marque t b visites liste
;;  


let calcul inter rues temps nbcar start tab liste=
  let trajets = Array.make nbcar [start] in
  let position = ref start in
  let tps = ref temps in
  let visites = Array.make rues false in
  let constante = 8 in
  let bloque = ref false in
  let trajet = ref [start] in
  for voiture = 0 to nbcar - 1 do
    printf "\nDébut du trajet pour la voiture %d.\n" voiture;
    position := start;
    tps := temps;
    bloque := false;
    trajet := [start];
    while !tps > 0 && not !bloque do
      printf "Voiture %d : temps restant : %d" voiture (!tps); print_newline();
      let (chemin,longueur,t) = calcul_chemin (!position) tab visites (!tps) constante liste in
(*      printf "Voiture %d : " voiture; print_liste (List.rev chemin); printf "Temps écoulé : %d\n" (!tps-t);*)
      if chemin = [] && t > 500 then (
	let (chemin2,_,t2) = random_chemin constante t (!position) tab in
	if t2 < 0 then bloque := true
	else (
	  marque (List.rev chemin2) (!position) visites liste;
	  trajet := chemin@(!trajet);
	  position := List.hd (!trajet);
	  tps := t2;
	);
      )
      else (
	if chemin = [] || t < 0 then bloque := true
	else (
	  marque (List.rev chemin) (!position) visites liste;
	  trajet := chemin@(!trajet);
	  position := List.hd (!trajet);
	  tps := t;
	);
      );
    done;
    trajets.(voiture) <- List.rev (!trajet);
  done;
trajets;;



let () =
  Random.self_init ();
  let (inter,rues,temps,nbcar,start,liste) = charge_fichier "paris_54000.txt" in
  let tab = tableau_adj liste inter in
  let trajets = calcul inter rues temps nbcar start tab liste in
  ecrit_fichier trajets
;;

