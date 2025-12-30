## Preamble

See [testschema.ml][] for the schema definition.
```ocaml
# open Caqti.Template
# open Caqti_schema
# open Caqti_schema_test.Testschema
# #install_printer Caqti.Template.Request.pp
```

## Schema Creation

The schema can be created with the following request template:
```ocaml
# Schema.init schema
- : (unit, unit, Row_mult.zero) Request.t =
(unit -->. unit) {|CREATE SCHEMA testschema;
                   CREATE TABLE testschema.user (uid int NOT NULL, name string NOT NULL, shell string NOT NULL, primary_gid int NOT NULL, PRIMARY KEY (uid));
                   CREATE TABLE testschema.group (gid int NOT NULL, name string NOT NULL, PRIMARY KEY (gid));
                   ALTER TABLE testschema.user ADD CONSTRAINT user_primary_gid_fkey FOREIGN KEY (primary_gid) REFERENCES testschema.group (gid)|}
```

## Queries and Updates

A subrow is a mapping between an OCaml datatype and one or more columns of a
table.  It can be used quite directly to generate request templates:
```ocaml
# Table.fetch user User.uid_key @@
    Subrow.create
      (fun name shell -> Ok (name, shell))
      User.[name, fst; shell, snd];;
- : (int, string * string, unique) Request.t =
((int) -->? (string × string)) {|SELECT name, shell FROM testschema.user WHERE uid = $1|}
```
However, it may be desirable to map full tables to custom types, so let's
define two types corresponding to our schema,
```ocaml
# type user = {
    uid: int;
    name: string;
    shell: string;
    primary_gid: int;
  }
  type group = {
    gid: int;
    name: string;
  }
type user = { uid : int; name : string; shell : string; primary_gid : int; }
type group = { gid : int; name : string; }
```
and corresponding subrows,
```ocaml
# let user_subrow =
    Subrow.create
      (fun uid name shell primary_gid -> Ok {uid; name; shell; primary_gid})
      User.[
        uid, (fun u -> u.uid);
        name, (fun u -> u.name);
        shell, (fun u -> u.shell);
        primary_gid, (fun u -> u.primary_gid);
      ]
  let group_subrow =
    Subrow.create
      (fun gid name -> Ok {gid; name})
      Group.[
        gid, (fun g -> g.gid);
        name, (fun g -> g.name);
      ]
val user_subrow : (User.table, user, Row_mult.zero_or_more) Subrow.t =
  <abstr>
val group_subrow : (Group.table, group, Row_mult.zero_or_more) Subrow.t =
  <abstr>
```
This allows easy construction of different kinds of request templates:
```ocaml
# Table.fetch user User.uid_key user_subrow
- : (int, user, unique) Request.t =
((int) -->? (int × string × string × int)) {|SELECT uid, name, shell, primary_gid FROM testschema.user WHERE uid = $1|}
# Table.insert user user_subrow
- : (user, unit, Row_mult.zero) Request.t =
((int × string × string × int) -->. unit) {|INSERT INTO testschema.user VALUES ($1, $2, $3, $4)|}
# Table.delete user User.uid_key
- : (int, unit, Row_mult.zero) Request.t =
((int) -->. unit) {|DELETE FROM testschema.user WHERE uid = $1|}
# Table.update user User.uid_key user_subrow
- : (int * user, unit, Row_mult.zero) Request.t =
(((int) × (int × string × string × int)) -->. unit) {|UPDATE testschema.user SET uid = $5, name = $6, shell = $7, primary_gid = $8 WHERE uid = $1|}
# Table.update user User.uid_key (Subrow.of_column User.shell)
- : (int * string, unit, Row_mult.zero) Request.t =
(((int) × (string)) -->. unit) {|UPDATE testschema.user SET shell = $2 WHERE uid = $1|}
```
