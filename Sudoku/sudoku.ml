
open Core.Std

let print_sudoku m =

  let module Array = Core_kernel.Core_array in
  assert (Array.length m = 9) ; 
  assert (Array.length m.(0) = 9) ;
  
  let print_line l = 
  Array.iteri
  ~f:(fun i x -> if i=8 then  printf "%d\n" x else printf "%d " x) l
  in
  Array.iter ~f:print_line m


let make_base_sudoku () = 

  let module Array = Core_kernel.Core_array in
  let make_line i = Array.init 9 ~f:(fun j -> (j+i) mod 9 + 1) in
  Array.init 9 ~f:(fun i -> make_line i)


let swap_columns m j j' =
  for i=0 to 8 do 
    let transfert = m.(i).(j) in
    m.(i).(j) <- m.(i).(j');
    m.(i).(j') <- transfert
  done


let make_sudoku () = 
  
  let m = make_base_sudoku () in
  
  let random = Core.Array_permute.Random.State.make_self_init () in
  Core.Array_permute.permute ~random_state:random m ;
 
  let columns = Core_kernel.Core_array.init 9 ~f:(fun i -> i) in 
  let random = Core.Array_permute.Random.State.make_self_init () in
  Core.Array_permute.permute ~random_state:random columns ;
  for j=0 to 8 do
    swap_columns m j columns.(j)
  done;
  m

let check_line m i =
  let module Array = Core_kernel.Core_array in
  assert (Array.length m = 9) ; 
  assert (Array.length m.(i) = 9) ;

  let line = m.(i) in
  let already_seen = Array.create ~len:9 false in
  let result = ref true in

  for j=0 to 8 do 
    result := !result && not already_seen.(line.(j)-1) ;
    already_seen.(line.(j)-1) <- true
  done;
  !result


let check_column m j =
  let module Array = Core_kernel.Core_array in
  assert (Array.length m = 9) ; 
  assert (Array.length m.(0) = 9) ;
  
  let already_seen = Array.create ~len:9 false in
  let result = ref true in

  for i=0 to 8 do
    result := !result && not already_seen.(m.(i).(j)-1) ;
    already_seen.(m.(i).(j)-1) <- true
  done;
  !result
  

let is_correct m = 
  
  let module Array = Core_kernel.Core_array in
  assert (Array.length m = 9) ; 
  assert (Array.length m.(0) = 9) ;

  let result = ref true in

  for ij = 0 to 8 do
    result := !result && (check_line m ij) && (check_column m ij)
  done;
  !result


    

let () =
  for k=0 to 10 do
    let sudoku = make_sudoku () in 
    print_sudoku sudoku ;
    if is_correct sudoku then printf "correct\n\n" else printf "incorrect\n\n"
  done
