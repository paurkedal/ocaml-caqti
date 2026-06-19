## `Caqti.Template.Row`

```ocaml
# open Caqti.Template;;
# open Caqti.Templater;;
# let printrow t = Format.printf "@[%a@]@." (Row.pp t);;
val printrow : 'a Row_type.t -> 'a -> unit = <fun>
# printrow T.unit ();; (* What should we print here? *)
- : unit = ()
# printrow T.(t3 int int int) (1, 2, 3);;
1, 2, 3
- : unit = ()
# printrow T.(t2 int (t2 int int)) (1, (2, 3));;
1, 2, 3
- : unit = ()
# printrow T.(t2 (t2 int int) int) ((1, 2), 3);;
1, 2, 3
- : unit = ()
# printrow T.(t7 bool int16 int32 int64 float string ptime)
    (true, 1, 2l, 3L, 4.1, "a string", Ptime.v (20623, 0L));;
true, 1, 2l, 3L, 4.1, "a string", 2026-06-19T00:00:00Z
- : unit = ()
```
