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

let pr_pair pair =
    Printf.printf "(%d, %d)\n" (fst pair) (snd pair);;

let get_neigh_coords coord =
    let x = fst coord in
    let y = snd coord in
    match coord with
    | (1,1) -> [(x,y+1);(x+1,y);(x+1,y+1)]
    | (a,1) when a = boardSize -> [(x-1,y);(x-1,y+1);(x,y+1)]
    | (1,b) when b = boardSize -> [(x,y-1);(x+1,y-1);(x+1,y)]
    | (a,b) when a = boardSize && b = boardSize -> [(x-1,y);(x-1,y-1);(x,y-1)]
    | (a,_) when a = boardSize -> [(x-1,y-1);(x,y-1);(x-1,y);(x-1,y+1);(x,y+1)]
    | (_,b) when b = boardSize -> [(x-1,y-1);(x,y-1);(x+1,y-1);(x-1,y);(x+1,y)]
    | (_,1) -> [(x-1,y);(x-1,y+1);(x,y+1);(x+1,y);(x+1,y+1)]
    | (1,_) -> [(x,y-1);(x+1,y-1);(x+1,y);(x,y+1);(x+1,y+1)]
    | _ -> [(x-1,y-1);(x,y-1);(x+1,y-1);(x-1,y);(x+1,y);(x-1,y+1);(x,y+1);(x+1,y+1)];;

let has_adjacent_opponent x y pl brd =
    let test_cases = get_neigh_coords (x,y) in
    List.iter pr_pair test_cases;;

let has_player_in_line x y pl brd =
    print_string "todo as well\n";;


(* sprawdza czy podane wspolrzedne ruchu nie wykraczaja poza plansze *)
let check_pos x y =
  x >= 0 && 
  y >= 0 && 
  y < Array.length board && 
  x < Array.length board.(0)
;;


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


let do_move p = 
	let quit_loop = ref false in
		while not !quit_loop do
			Printf.printf "Player %c move: " p;
			let move = read_int() in 
		    	let row = (move / 10) - 1 in 
				    let col = (move mod 10) - 1 in
				    	if (check_pos row col) then (
				    		board.(row).(col) <- p;
				    		quit_loop := true
				    	)
		done;;

while true do
    print_board board;
    do_move 'X';
    print_board board;
    do_move 'Y';
done;
