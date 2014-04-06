
open Printf
open Pervasives
open List


let read_int s pos =
    let n = String.length s in
    let rec traite accu i =
        if i >= n || s.[i]=' ' then
            (int_of_string accu,i+1)
        else traite (accu^Char.escaped s.[i]) (i+1) in
    traite "" pos

let read_float s pos =
    let n = String.length s in
    let rec traite accu i =
        if i >= n || s.[i]=' ' then
            (float_of_string accu,i+1)
        else traite (accu^Char.escaped s.[i]) (i+1) in
    traite "" pos

let split s =
    let n = String.length s in
    let rec traite accu curs pos =
        if pos >= n then
            Array.of_list (rev (int_of_string curs :: accu))
        else if s.[pos]=' ' then
            traite (int_of_string curs :: accu) "" (pos+1)
        else traite accu (curs^Char.escaped s.[pos]) (pos+1) in
    traite [] "" 0

let split_float s =
    let n = String.length s in
    let rec traite accu curs pos =
        if pos >= n then
            Array.of_list (rev (float_of_string curs :: accu))
        else if s.[pos]=' ' then
            traite (float_of_string curs :: accu) "" (pos+1)
        else traite accu (curs^Char.escaped s.[pos]) (pos+1) in
    traite [] "" 0


(* lecture de l'entrée *)
let entree() =
    let positions = ref [] in
    let input = open_in (try Sys.argv.(1) with _ -> "test.txt") in
    let numbers = input_line input in
    let n,p1 = read_int numbers 0 in
    let m,p2 = read_int numbers p1 in
    let t,p3 = read_int numbers p2 in
    let c,p4 = read_int numbers p3 in
    let s,_ = read_int numbers p4 in
    let rec lecture accu countdown =
        if countdown > 0 then
        (
            positions := (input_line input) :: !positions;
            lecture accu (countdown-1)
        )
        else
        (
            let s = try input_line input with _ -> "" in
            if s = "" then Array.of_list (rev accu)
            else lecture (split s :: accu) 0
        ) in
    let lect = lecture [] n in
    close_in input;
    let v = Array.of_list(rev !positions) in
    let posmagueule =
        Array.init n (fun i -> split_float v.(i)) in

    lect,posmagueule,n,m,t,c,s


(* l'input *)
let input,positions,n,m,t,c,s = entree()




(* fonctions d'écriture *)
let out = open_out "output.txt"
let fermer () = close_out out
let write s = output_string out s



(* C'est parti mon kiki *)
let rues_accessibles inters timeout =
    let rec traite accu pos =
        if pos = m then accu
        else if input.(pos).(0)=inters && input.(pos).(3)<= timeout then
            traite ((input.(pos).(1),pos)::accu) (pos+1)
        else if input.(pos).(1)=inters
                && input.(pos).(2)=2
                && input.(pos).(3) <= timeout then
            traite ((input.(pos).(0),pos)::accu) (pos+1)
        else traite accu (pos+1) in
    traite [] 0

let random_rue inters timeout =
    let l = rues_accessibles inters timeout in
    if l = [] then -1,-1
    else (Array.of_list l).(Random.int (List.length l))

(* grosse merde *)
let meilleure_rue distance inters timeout =
    let l = rues_accessibles inters timeout in
    if l = [] then -1,-1
    else
        let maxi one two =
            if distance (fst one) > distance (fst two) then one else two in
        fold_left maxi (hd l) (tl l)


(* Trouve une rue où on peut aller à partir de inters, sans optimisation *)
let trouve_porc inters timeout =
    (*let rec traite pos =
        if pos = m then -1,-1
        else if input.(pos).(0)=inters && input.(pos).(3) <= timeout then
            input.(pos).(1),pos
        else if (input.(pos).(1)=inters && input.(pos).(2)=2)
                && input.(pos).(3) <= timeout then
            input.(pos).(0),pos
        else traite (pos+1) in
    traite 0*)

    random_rue inters timeout






let glouton_chelou () =
    (* indique si le chemin est libre *)
    let libres = Array.make m true in
    (* la suite des chemins *)
    let chemins = Array.make c [] in
    let distance inters =
        let carre sq = sq*.sq in
        carre (positions.(inters).(0)-.positions.(s).(0)) +.
        carre (positions.(inters).(1)-.positions.(s).(1)) in
    let score timeout (go,pos) =
        let accessibles = rues_accessibles go timeout in
        let oks time (intrs,street) =
            float_of_int(
                List.length (
                    filter (fun (_,rue) -> libres.(rue))
                        (rues_accessibles intrs time))) in
        let prof1 = oks timeout (go,pos) in
        let prof2 = fold_left (fun ac (g,p) -> ac+.oks timeout (g,p)) 0. accessibles in

        (if libres.(pos) then
            float_of_int input.(pos).(4) else 2.)*.
        distance go *. distance go *.
        (prof1 *. prof1 /.3. +. prof2) /.
        (float_of_int input.(pos).(2) *.
            float_of_int input.(pos).(3)) in


    (* fonction de comparaison de score: compare les rapports *)
    let gros timeout (a,i) (b,j) =
        (*input.(j).(4)*input.(i).(3) < input.(j).(3)*input.(i).(4)*)
        (*input.(i).(3)<input.(i).(3)*)
        score timeout (a,i) > score timeout (b,j) in

    let next inters timeout =
        let rec traite accu pos =
            if pos = m && accu <> [] then
                fold_left
                    (fun (a,i) (b,j) -> if gros timeout (b,j) (a,i) then b,j else a,i)
                    (hd accu) (tl accu)
            else if pos = m then
                trouve_porc inters timeout
                (*meilleure_rue distance inters timeout*)
            else if input.(pos).(0)=inters
                    && libres.(pos)
                    && timeout >= input.(pos).(3) then
                traite ((input.(pos).(1),pos) ::accu) (pos+1)
            else if (input.(pos).(1)=inters && input.(pos).(2)=2)
                    && libres.(pos)
                    && timeout >= input.(pos).(3) then
                traite ((input.(pos).(0),pos) ::accu) (pos+1)

            else traite accu (pos+1) in
        traite [] 0 in


    let rec avance accu timeout pos =
        if timeout < 0 then failwith "lol";
        let suivant,rue = next pos timeout in
        if suivant = -1 then rev (pos::accu)
        else
        (
            libres.(rue) <- false;
            avance (pos :: accu) (timeout-input.(rue).(3)) suivant
        ) in

    for i=0 to c-1 do
        chemins.(i) <- avance [] t s
    done;

    (* écriture dans l'output *)
    write (sprintf "%d\n" c);
    for i=0 to c-1 do
        write (sprintf "%d\n" (List.length chemins.(i)));
        List.iter (fun pos -> write (sprintf "%d\n" pos)) chemins.(i)
    done





let glouton_chelou_synchro () =
    (* indique si le chemin est libre *)
    let libres = Array.make m true in
    (* la suite des chemins *)
    let chemins = Array.make c [] in
    let distance inters =
        let carre sq = sq*.sq in
        carre (positions.(inters).(0)-.positions.(s).(0)) +.
        carre (positions.(inters).(1)-.positions.(s).(1)) in
    let score timeout (go,pos) =
        let accessibles = rues_accessibles go timeout in
        let oks time (intrs,street) =
            float_of_int(
                List.length (
                    filter (fun (_,rue) -> libres.(rue))
                        (rues_accessibles intrs time))) in
        let prof1 = oks timeout (go,pos) in
        let prof2 = fold_left (fun ac (g,p) -> ac+.oks timeout (g,p)) 0. accessibles in

        (if libres.(pos) then
            float_of_int input.(pos).(4) else 2.)*.
        distance go *. distance go *.
        (prof1 *. prof1 /.3. +. prof2) /.
        (float_of_int input.(pos).(2) *.
            float_of_int input.(pos).(3)) in


    (* fonction de comparaison de score: compare les rapports *)
    let gros timeout (a,i) (b,j) =
        (*input.(j).(4)*input.(i).(3) < input.(j).(3)*input.(i).(4)*)
        (*input.(i).(3)<input.(i).(3)*)
        score timeout (a,i) > score timeout (b,j) in

    let next inters timeout =
        let rec traite accu pos =
            if pos = m && accu <> [] then
                fold_left
                    (fun (a,i) (b,j) -> if gros timeout (b,j) (a,i) then b,j else a,i)
                    (hd accu) (tl accu)
            else if pos = m then
                trouve_porc inters timeout
                (*meilleure_rue distance inters timeout*)
            else if input.(pos).(0)=inters
                    && libres.(pos)
                    && timeout >= input.(pos).(3) then
                traite ((input.(pos).(1),pos) ::accu) (pos+1)
            else if (input.(pos).(1)=inters && input.(pos).(2)=2)
                    && libres.(pos)
                    && timeout >= input.(pos).(3) then
                traite ((input.(pos).(0),pos) ::accu) (pos+1)

            else traite accu (pos+1) in
        traite [] 0 in

    let les_chemins = Array.make c [] in
    let timeouts = Array.make c t in
    let poss = Array.make c s in
    let locked = Array.make c false in


    let avance i =
        print_endline (sprintf "le voiture %d avance" i);
        let timeout = timeouts.(i) in
        let pos = poss.(i) in
        let suivant,rue = next pos timeout in
        if suivant = -1 then
        (
            les_chemins.(i) <- rev(pos::les_chemins.(i));
            locked.(i) <- true
        )
        else
        (
            libres.(rue) <- false;
            les_chemins.(i) <- pos :: les_chemins.(i);
            timeouts.(i) <- timeouts.(i) - input.(rue).(3);
            poss.(i) <- suivant
        ) in

    let tt = Array.make c true in
    while locked <> tt do
        for i=0 to c do
            if not locked.(i) then avance i
        done
    done;


    for i=0 to c-1 do
        chemins.(i) <- les_chemins.(i)
    done;

    (* écriture dans l'output *)
    write (sprintf "%d\n" c);
    for i=0 to c-1 do
        write (sprintf "%d\n" (List.length chemins.(i)));
        List.iter (fun pos -> write (sprintf "%d\n" pos)) chemins.(i)
    done







(* application *)
let _ =
    Random.self_init();
    glouton_chelou();
    fermer()

