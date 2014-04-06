
open Core.Std

let print_sudoku m =

  let module Array = Core_kernel.Core_array in
  
  let print_line l = 
  Array.iteri
  ~f:(fun i x -> if i=8 then  printf "%d\n" x else printf "%d " x) l
  in
  Array.iter ~f:print_line m



let check_line m i ~tolerate_zeros =
  let module Array = Core_kernel.Core_array in

  let line = m.(i) in
  let already_seen = Array.create ~len:9 false in
  let result = ref true in

  for j=0 to 8 do 
    result := !result && ( (line.(j)<>0 && not already_seen.(line.(j)-1)) ||
      (line.(j)=0 && tolerate_zeros) ) ;
    if line.(j)<>0 then already_seen.(line.(j)-1) <- true
  done;
  !result


let check_column m j ~tolerate_zeros =
  let module Array = Core_kernel.Core_array in
  
  let already_seen = Array.create ~len:9 false in
  let result = ref true in

  for i=0 to 8 do
    result := !result && ( (m.(i).(j)<>0 && not already_seen.(m.(i).(j)-1)) ||
       (m.(i).(j)=0 && tolerate_zeros) );
    if m.(i).(j)<>0 then (already_seen.(m.(i).(j)-1)) <- true
  done;
  !result

let check_block m block_nb ~tolerate_zeros = 
  let already_seen = Core_kernel.Core_array.create ~len:9 false in
  let result = ref true in
  
  let line = 3*(block_nb/3) in
  let column = 3*(block_nb mod 3) in

  for l_offset=0 to 2 do
    for c_offset=0 to 2 do
      let i = line+l_offset in
      let j = column+c_offset in
      result := !result && ( (m.(i).(j)<>0 && not already_seen.(m.(i).(j)-1)) ||
       (m.(i).(j)=0 && tolerate_zeros) ); 
      if m.(i).(j)<>0 then (already_seen.(m.(i).(j)-1)) <- true   
    done;
  done;
  !result


let is_correct m ~tolerate_zeros = 
  
  let module Array = Core_kernel.Core_array in

  let result = ref true in

  for ij = 0 to 8 do
    result := !result && (check_line m ij ~tolerate_zeros) && 
      (check_column m ij ~tolerate_zeros) && (check_block m ij ~tolerate_zeros)
  done;
  !result

let rec read_line_list m line_nb column_nb nb_list =
  match nb_list with
  | [] -> ()
  | hd::tl ->
      m.(line_nb).(column_nb) <- Core_kernel.Core_int.of_string hd ;
      read_line_list m line_nb (column_nb+1) tl

let rec read_lines m line_nb =
  match (Core_kernel.In_channel.input_line stdin ~fix_win_eol:true) with
  | None -> ()
  | Some s -> 
    if s <> "" then begin
      read_line_list m line_nb 0 (Core_kernel.Core_string.split s ~on:' ') ;
      read_lines m (line_nb+1)
    end


let make_empty_sudoku () =
  let module Array = Core_kernel.Core_array in
  let make_line _ = Array.create ~len:9 0 in
  Array.init 9 ~f:(fun i -> make_line i)



let read_sudoku () =
  let m = make_empty_sudoku () in
  read_lines m 0 ;
  m
    

	
(* begin at case_number = 0 *)
let rec solve m case_number =

  if case_number=81 then is_correct m ~tolerate_zeros:false (*final check*)
  else begin
    if not (is_correct m ~tolerate_zeros:true) then false
    else begin
      let line = case_number/9 in
      let column = case_number mod 9 in
    
      if m.(line).(column)<>0 then solve m (case_number+1)
      else begin
	(try_with m case_number 1)||
	(try_with m case_number 2)||
	(try_with m case_number 3)||
	(try_with m case_number 4)||
	(try_with m case_number 5)||
	(try_with m case_number 6)||
	(try_with m case_number 7)||
	(try_with m case_number 8)||
	(try_with m case_number 9)
      end
    end
  end

and try_with m case_number value =
  
  let line=case_number/9 in
  let column=case_number mod 9 in
  
  m.(line).(column) <- value ;

  if solve m (case_number+1) then true
  else begin 
    m.(line).(column) <- 0 ; 
    false
  end





let make_sudoku () = 

  let module Array = Core_kernel.Core_array in
  
  let m = make_empty_sudoku () in
  let first_line = Core_kernel.Core_array.init 9 ~f:(fun i -> i+1) in
  
  let random = Core.Array_permute.Random.State.make_self_init () in
  Core.Array_permute.permute ~random_state:random first_line ;
 
  m.(0) <- first_line ;
  if solve m 0 then m else make_empty_sudoku ()

let rec make_holes m nb_of_holes =
  if nb_of_holes > 0 then begin
    let i=Core_kernel.Core_random.int 9 in
    let j=Core_kernel.Core_random.int 9 in
    m.(i).(j) <- 0 ;
    make_holes m (nb_of_holes-1)
  end


let rec make_test_cases nb =
  if nb>0 then begin
    Core_kernel.Core_random.self_init ();
    let m = make_sudoku () in
    let nb_of_holes = Core_kernel.Core_random.int 82 in
    make_holes m nb_of_holes ;
    print_sudoku m ;
    printf "\n" ;
    make_test_cases (nb-1)
  end	   





let () = 
  for i=1 to 20 do 
    print_sudoku (make_sudoku ());
    printf "\n"
  done;
