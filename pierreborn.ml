
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

let rec pop n=function
  |[x]->x
  |x::q when n=0-> x
  |x::q->pop (n-1) q;;

let rec first_pos timer = function
  |[]->[]
  |(b,c,l)::q when timer>=c ->  [(b,c,l)]
  |(b,c,l)::q ->first_pos timer q;;

let rec trajet timer from  n =
  if timer<=0 then [] else
    begin
      let (b,c,l)=pop n (tabadj.(from)) in
      if timer-c>=0 then
	begin
	  b::(trajet (timer-c) b 0)
	end
      else
	begin
	  let l=first_pos timer (tabadj.(from)) in
	  match l with
	  |[]->[]
	  |[(b,c,l)]->
	    b::(trajet (timer-c) b 0)
	end
    end;;


(***********************)


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
      let pathw= (trajet t s i) in
      res.(i)<-s::pathw;
    done;
    res;;


let output ans=
  print_endline (Printf.sprintf "%d" nbcar);
  output_string ans (Printf.sprintf "%d\n" nbcar);
  for i=0 to nbcar-1 do
    let pathw= (trajet t s i) in
    let path=s::pathw in
    print_endline (Printf.sprintf  "%d" (List.length path));
    output_string ans (Printf.sprintf "%d\n" (List.length path));
    car_out ans (path);
  done;;

let rank=850000;;

let ans= open_out "ansborn" ;;
let _=
  
  print_endline (Printf.sprintf "Nouveau score : pas calculé") ;
  output ans;
  close_out ans;;
