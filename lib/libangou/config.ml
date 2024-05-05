(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of oangou: a content encryptor                                           *)
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

let angou_name = "oangou"
let version = "0.1.0-1"
let ( / ) = Filename.concat
let xdg = Xdg.create ~env:Sys.getenv_opt ()
let xdg_data = Xdg.data_dir xdg
let xdg_config = Xdg.config_dir xdg
let xdg_state = Xdg.state_dir xdg
let angou_share_dir = xdg_data / angou_name
let angou_state_dir = xdg_state / angou_name
let angou_keys_file_name = Printf.sprintf ".%src" angou_name
let angou_keys_file = angou_share_dir / angou_keys_file_name
let is_angou_initialized () = Util.FileSys.file_exists angou_keys_file

let check_initialized () =
  match is_angou_initialized () with
  | false ->
      raise @@ Error.Exn.angou_not_configured ()
  | true ->
      ()
