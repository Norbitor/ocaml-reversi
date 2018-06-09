open Printf
open Scanf


(*
Legalny ruch:
 - pionek stawiany jest w linii prostej (pion,poziom) lub ukosnej względem
   innego pionku gracza
 - pionek musi być postawiony obok pionku przeciwnika
 - postawienie pionka powoduje zmiane koloru (u nas X na O lub odwrotnie)
   pionków w całej wyznaczonej linii
 - jeśli gracz nie ma legalnych ruchów, traci kolejkę na rzecz przeciwnika
*)

let player = ref 'X';;
let boardSize = 8;;
let board = Array.make_matrix boardSize boardSize '_';;


let init_board brd =
    let xmid1 = (boardSize / 2 - 1, boardSize / 2 - 1) in
    let xmid2 = ((fst xmid1) + 1, (snd xmid1) + 1) in
    let omid1 = (boardSize / 2 - 1, boardSize / 2) in
    let omid2 = ((fst omid1) + 1, boardSize / 2 - 1) in
    brd.(fst xmid1).(snd xmid1) <- 'X';
    brd.(fst xmid2).(snd xmid2) <- 'X';
    brd.(fst omid1).(snd omid1) <- 'O';
    brd.(fst omid2).(snd omid2) <- 'O';;

    (* przypadki testowe przylegania przeciwnika
       (x-1, y-1) (x, y-1) (x+1, y-1)
       (x-1, y) (x+1, y)
       (x-1, y+1) (x, y+1), (x+, y+1)
    *)

let opponent pl =
   if pl = 'X' then 'O' else 'X';;

let has_adjacent_opponent x y pl brd =
    let test_cases = [(x-1,y-1);(x,y-1);(x+1,y-1);(x-1,y);(x+1,y);(x-1,y+1);(x,y+1);(x+1,y+1)] in
    print_string "iterate through list\n";;


let print_board b =
    printf "   1 2 3 4 5 6 7 8\n";
    printf "  ----------------\n";
    for i = 0 to Array.length board - 1 do
        printf "%d| " (i+1);
        for j = 0 to Array.length board.(i) - 1 do
            printf "%c " board.(i).(j)
        done;
        printf "\n";
    done;
    printf "  -----------\n";;


init_board board;;

has_adjacent_opponent 1 2 3 4;;

while true do
    print_board board;
    Printf.printf "Player %c move: " !player;
    let move = read_int() in 
    let row = (move / 10) - 1 in 
    let col = (move mod 10) - 1 in 
        board.(row).(col) <- !player;
done;
