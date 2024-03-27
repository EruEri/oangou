(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of oangou: a file encryptor                                              *)
(* Copyright (C) 2024 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* oangou is free software: you can redistribute it and/or modify it under the terms          *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* oangou is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;        *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with oangou.       *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

(**
    [seed ()] generates a 16 byte sequence of random byte using [Random.bits64]
*)
let seed () =
  let bytes = Bytes.make 16 '\x00' in
  let h = Random.bits64 () in
  let l = Random.bits64 () in
  let () = Bytes.set_int64_ne bytes 0 h in
  let () = Bytes.set_int64_ne bytes 0 l in
  Cstruct.of_bytes bytes
