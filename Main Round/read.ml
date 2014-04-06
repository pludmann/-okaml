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

let split s =
    let n = String.length s in
    let rec traite accu curs pos =
        if pos >= n then
            Array.of_list (rev (int_of_string curs :: accu))
        else if s.[pos]=' ' then
            traite (int_of_string curs :: accu) "" (pos+1)
        else traite accu (curs^Char.escaped s.[pos]) (pos+1) in
    traite [] "" 0


(* lecture de l'entrée *)
let entree() =
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
            ignore(input_line input);
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
    lect,n,m,t,c,s


(* l'input *)
let input,n,m,t,c,s = entree()


(* fonctions d'écriture *)
let out = open_out "output.txt"
let fermer () = close_out out
let write s = output_string out s



(* *)


(* application *)
let _ =
    printf "\n%d,%d,%d,%d,%d\n\n" n m t c s;
    for i=0 to m-1 do
        for j=0 to 4 do
            printf "%d " input.(i).(j)
        done;
        print_endline ""
    done;
    fermer() 
