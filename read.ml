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
