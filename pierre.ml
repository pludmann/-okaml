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


let (n,m,t,nbcar,s,streets)=charge_fichier "paris_54000.txt";;



(********************************)

let rec ratio_order (a1,b2,d1,c1,l1) (a2,b2,d2,c2,l2) =
( float_of_int(l1) /. float_of_int(c1) )>( float_of_int(l2) /. float_of_int(c2) );;

let rStreets = Sort.list ratio_order streets;;


let rec cost_order (a1,b2,d1,c1,l1) (a2,b2,d2,c2,l2) =
(c1)<=(c2);;

let cStreets = Sort.list cost_order streets;;

(**********************************)

type next = Inter of int|Stop;;

let unseens=ref rStreets;;

let lastissue=cStreets;;

let rec seek_dir from = function
  |[]-> (Stop,0,[])
  |(a,b,d,c,l)::q when d=1&a=from->(Inter(b),c,q)
  |(a,b,d,c,l)::q when d=1&a<>from->
    let (nexti,ci,uns)=seek_dir from q in
    (nexti,ci,(a,b,d,c,l)::uns)
  |(a,b,d,c,l)::q when d=2&a=from->(Inter(b),c,q)
  |(a,b,d,c,l)::q when d=2&b=from->(Inter(a),c,q)
  |(a,b,d,c,l)::q when d=2&a<>from&b<>from ->
    let (nexti,ci,uns)=seek_dir from q in
    (nexti,ci, (a,b,d,c,l)::uns);;

let rec last_dir from = function
  |[]->failwith "Pas possible"
  |(a,b,d,c,l)::q when d=1&a=from->(b,c)
  |(a,b,d,c,l)::q when d=1&a<>from->last_dir from q
  |(a,b,d,c,l)::q when d=2&a=from->(b,c)
  |(a,b,d,c,l)::q when d=2&b=from->(a,c)
  |(a,b,d,c,l)::q when d=2&a<>from&b<>from -> last_dir from q;;


let rec trajet timer from =
if timer=0 then []
else
  let (nexti,c,uns)=seek_dir from !unseens in
  match nexti with
  |(Stop)->let (nexti,c)=last_dir from cStreets in
	 if timer-c>=0
	 then
	   begin
	     nexti::(trajet (timer-c) nexti);
	   end
	 else []
  |(Inter(i))->if timer-c>=0
    then
      begin
	unseens:=uns;
	i::(trajet (timer-c) i);
      end
    else
      begin
	let (nexti,c)=last_dir from cStreets in
	if timer-c>=0
	then
	  begin
	    nexti::(trajet (timer-c) nexti);
	  end
	else []
      end;;

let ans =  open_out "ans";;


let rec car_out = function
  |[]->()
  |x::q->output_string ans (Printf.sprintf "%d\n" x);;


let output ()=
  output_string ans "coucou";
  output_string ans (Printf.sprintf "%d\n" nbcar);
  for i=1 to nbcar
  do
    let pathw= (trajet t s) in
    let path=s::pathw in
    output_string ans (Printf.sprintf "%d\n" (List.length path));
    car_out path;
  done;;

let _=output();;
