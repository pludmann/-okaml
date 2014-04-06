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


let (doodle,n,m)=charge_fichier "doodle.txt";;

let output = open_out "answer.txt";;

let testcolor r c s=
  let test = true in
for i=r-s to r+s do
for j=c-s to c+s do
test:=test&doodle.(i).(j)
done;
done;
!test;;

let bounds r c rL rR cU cD =
  let s=ref 0 in
while (testcolor r c !s)& r-!s>rL& c-!s>cU)& r+!s<rR& c+!s<cD
do incr s done;
!s;;

let compteur = ref 0;;

let rec paint rL rR cU cD =
incr compteur;

if rR<>rL&cU<>cD then
  begin

    let r1=(rR-rL)/3
    and c=(cD-cU)/2 in
    let r2=2*r1 in
    let bool1 = doodle.(r1).(c)
    and bool2=doodle.(r2).(c)
    and s1=bounds r1 c rL rR cU cD
    and s2=bounds r2 c rL rR cU cD in

    if bool1
    then    output_string output (Printf.sprintf "PAINTSQ %d %d %d\n" r1 c s1);

 if bool2
    then
    output_string output (Printf.sprintf "PAINTSQ %d %d %d\n" r2 c s2);


    paint rL rl cU cu;
    paint rl rr cU cu;
    paint rr rR cU cu;
    paint rL rl cu cd;
    paint rl rr  cu cd;
    paint rL rl cd cD;
    paint rl rr cd cD;
    paint rr rR cd cD;

  end

else
  begin 
    if rL=rR then 
      for c=cU to cD do
	if doodle.(rL).(c)
	then  output_string output "PAINTSQ "
	else output_string output "ERASECELL ";
	output_string output (Printf.sprintf "%d %d %d\n" rL cU 0);
      done; 
    
 else (*cU=cD*) 
  for r=rL to rR do
    if doodle.(rL).(cU)
    then  output_string output "PAINTSQ "
    else output_string output "ERASECELL ";
    output_string output (Printf.sprintf "%d %d %d\n" rL cU 0);
  done; 
 end;;


paint 0 (n-1) 0 (m-1);;

output_string output (Printf.sprintf "%d\n" !compteur);;
