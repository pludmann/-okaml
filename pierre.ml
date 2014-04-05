type next = Inter of int|Stop;;

unseens=ref rStreets;;

lastissue=cStreets;;

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
if timer=0 then ()
else
  let (nexti,c,uns)=seek_dir from in
  match nexti with
  |(Stop)->let (nexti,c)=last_dir from in
	 if timer-c>=0
	 then
	   begin
	     output_string output (Printf.sprintf "%d\n" nexti);
	     trajet (timer-c) nexti;
	   end
	 else ()
  |(Next(i))->if timer-c>=0
    then
      begin
	unseens:=uns;
	output_string output (Printf.sprintf "%d\n" i);
	trajet (timer-c) i;
      end
    else
      begin
	let (nexti,c)=last_dir from in
	if timer-c>=0
	then
	  begin
	    output_string output (Printf.sprintf "%d\n" nexti);
	    trajet (timer-c) nexti;
	  end
	else ()
      end;;
