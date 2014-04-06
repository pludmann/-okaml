
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

let (n,m,t,nbcar,s,streets)=charge_fichier "paris_54000.txt";;

let tabadj=tableau_adj streets n;;


(**********************)

let unseens=ref streets;;

let rec pop n=function
  |[x]->x
  |x::q when n=0-> x
  |x::q->pop (n-1) q;;

let compt=ref 0

let rec aux res x y = function
  |[]->(res,[])
  |(a,b,d,c,l)::q when a=x&b=y->(res+l,q)
  |(a,b,d,c,l)::q when d=2&a=y&b=x->(res+l,q)
  |(a,b,d,c,l)::q -> let (resf,uns)=aux res x y q in
		     (resf,(a,b,d,c,l)::uns);;
let scoring x y =
  let (scor,uns)=aux 0 x y !unseens in
  unseens:=uns;
  scor;;

let rec trajet timer from =
  if timer<=0 or !compt>42 then ([],0) else
    begin
      let (b,c,l)=pop (Random.int (List.length (tabadj.(from)))) (tabadj.(from)) in
      if timer-c>=0 then
	begin
	  compt:=0;
	  let (traj,scor)=(trajet (timer-c) b) in
	  (b::traj,scor+(scoring from b))
	end
      else
	begin
	  incr compt;
	  trajet timer from;
	end
    end;;


(***********************)
 let ans= open_out "ansrand1" ;;


let rec car_out ans = function
  |[]->()
  |x::q->
    print_endline (Printf.sprintf  "%d" x);
    output_string ans (Printf.sprintf "%d\n" x);
    car_out ans q;;



let asol () =
    let res=[|[];[];[];[];[];[];[];[]|]
    and score=ref 0 in
    for i=0 to nbcar-1
    do
      compt:=0;
      let (pathw,scor)= (trajet t s) in
      res.(i)<-s::pathw;
      score:=!score+scor;
    done;
    (!score,res);;


let output ans res=
  print_endline (Printf.sprintf "%d" nbcar);
  output_string ans (Printf.sprintf "%d\n" nbcar);
  for i=0 to nbcar-1 do
    print_endline (Printf.sprintf  "%d" (List.length (res.(i))));
    output_string ans (Printf.sprintf "%d\n" (List.length (res.(i))));
    car_out ans (res.(i));
  done;;

let rank=1323248;;

let ans= open_out "ansborn" ;;
let _=
  let rres=ref [||]
  and rscore=ref 0 in
  while !rscore<rank do
    unseens:=streets;
    let (score,res)=asol () in
    rres:=res;
    rscore:=score;
    print_endline (Printf.sprintf "Nouveau score : %d" !rscore) 
  done;
output ans (!rres);
  close_out ans;;
