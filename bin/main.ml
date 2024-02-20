let square = .<fun x -> x * x>.
let rec spower square n x =
      if n = 0 then .<1>.
      else if n mod 2 = 0 then .<.~square .~(spower square (n/2) x)>.
      else .<.~x * .~(spower square (n-1) x)>.
    (* val spower : int -> int code -> int code = <fun> *)
      let spower7_code = .<fun x -> .~(spower square 7 .<x>.)>.;;
open Codelib;;
let _ = format_code Format.std_formatter (close_code spower7_code) in
()
