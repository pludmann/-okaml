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
  in
  aux l;
v;;
