open! Core
open Virtual_dom
open Async_kernel
open Bonsai.Let_syntax
open Js_of_ocaml
open Chinese_checkers_logic_library
open Game

module Id = struct
  let generate () =
    let t = (new%js Js.date_now)##getTime |> Js.to_float |> int_of_float in
    let r = Random.int 0x3ffffff in
    Printf.sprintf "%x-%x" t r
  ;;
end

module Player_label = struct
  let make idx = Printf.sprintf "Player %d" (idx + 1)
end

module Layout = struct
  let size = 1.0
  let w = Float.sqrt 3.0 *. size
  let h = 2.0 *. size
  let half_w = w /. 2.0
  let half_h = h /. 2.0

  let xy_of_axial (q, r) =
    let qf = Float.of_int q
    and rf = Float.of_int r in
    let x = Float.sqrt 3.0 *. (qf +. (rf /. 2.0)) *. size in
    let y = 1.5 *. rf *. size in
    x, y
  ;;

  module Bounds = struct
    type t =
      { cx : float
      ; cy : float
      ; span : float
      }
  end

  let uniform_bounds (cells : (Cell_position.t * 'a) list) : Bounds.t =
    let xs, ys =
      List.map cells ~f:(fun (pos, _) -> xy_of_axial (pos.q_coordinate, pos.r_coordinate))
      |> List.unzip
    in
    let min_x = Option.value ~default:0. (List.min_elt xs ~compare:Float.compare) in
    let max_x = Option.value ~default:1. (List.max_elt xs ~compare:Float.compare) in
    let min_y = Option.value ~default:0. (List.min_elt ys ~compare:Float.compare) in
    let max_y = Option.value ~default:1. (List.max_elt ys ~compare:Float.compare) in
    let min_x = min_x -. half_w
    and max_x = max_x +. half_w in
    let min_y = min_y -. half_h
    and max_y = max_y +. half_h in
    let cx = (min_x +. max_x) /. 2.0 in
    let cy = (min_y +. max_y) /. 2.0 in
    let span = Float.max (max_x -. min_x) (max_y -. min_y) in
    { cx; cy; span }
  ;;

  let normalize ~bounds:(b : Bounds.t) ~(fit : float) (x, y) =
    let nx = ((x -. b.cx) /. b.span *. fit) +. 0.5 in
    let ny = ((y -. b.cy) /. b.span *. fit) +. 0.5 in
    nx *. 100.0, ny *. 100.0
  ;;

  let cell_size_pct ~bounds:(b : Bounds.t) ~(fit : float) ~(cell_scale : float) =
    let cw = w /. b.span *. fit *. cell_scale *. 100.0 in
    let ch = h /. b.span *. fit *. cell_scale *. 100.0 in
    cw, ch
  ;;
end

module Firebase = struct
  module Config = struct
    let project = "ocaml-cttt"
    let key = "AIzaSyBsyzwDn-o2a47CAelN0kixWpFEryHuKGE"

    let base =
      Printf.sprintf
        "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents"
        project
    ;;

    let run_query =
      Printf.sprintf
        "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents:runQuery?key=%s"
        project
        key
    ;;

    (* single-document endpoints *)
    let lobby_doc id = Printf.sprintf "%s/lobby/%s?key=%s" base id key
    let game_doc id = Printf.sprintf "%s/game-state/%s?key=%s" base id key

    (* PATCH upsert endpoints with explicit update masks *)
    let lobby_upsert id =
      Printf.sprintf
        "%s/lobby/%s?key=%s&updateMask.fieldPaths=game_id&updateMask.fieldPaths=capacity&updateMask.fieldPaths=players&updateMask.fieldPaths=status&updateMask.fieldPaths=created_at"
        base
        id
        key
    ;;

    let game_upsert id =
      Printf.sprintf
        "%s/game-state/%s?key=%s&updateMask.fieldPaths=game_id&updateMask.fieldPaths=state&updateMask.fieldPaths=created_at"
        base
        id
        key
    ;;
  end

  module Http = struct
    type method_ =
      [ `GET
      | `POST
      | `PATCH
      ]

    let to_js = function
      | `GET -> Js.string "GET"
      | `POST -> Js.string "POST"
      | `PATCH -> Js.string "PATCH"
    ;;

    let request ~(meth : method_) ~(url : string) ?(body : string option) () =
      let open Async_kernel in
      Deferred.create (fun ivar ->
        let xhr = XmlHttpRequest.create () in
        xhr##_open (to_js meth) (Js.string url) Js._true;
        (match body with
         | Some _ ->
           xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json")
         | None -> ());
        xhr##.onreadystatechange
        := Js.wrap_callback (fun _ ->
             match xhr##.readyState with
             | XmlHttpRequest.DONE ->
               let status = xhr##.status in
               let resp =
                 Js.Opt.get xhr##.responseText (fun () -> Js.string "") |> Js.to_string
               in
               Ivar.fill ivar (status, resp)
             | _ -> ());
        ignore
          (xhr##send
             (match body with
              | None -> Js.null
              | Some b -> Js.Opt.return (Js.string b))))
    ;;
  end

  module Json = struct
    let get o k = Js.Unsafe.get o k
    let opt o = Js.Optdef.test (Js.Optdef.return o)
    let to_string_exn v = Js.to_string v
    let int_of_jsint v = v |> Js.to_string |> Int.of_string
  end

  module Lobby = struct
    type t =
      { game_id : string
      ; capacity : int
      ; players : string list
      ; status : string
      }
    [@@deriving sexp, equal]

    let encode_create ~(game_id : string) ~(capacity : int) ~(first_player : string) =
      Printf.sprintf
        {|{"fields":{
            "game_id":{"stringValue":"%s"},
            "capacity":{"integerValue":"%d"},
            "players":{"arrayValue":{"values":[{"stringValue":"%s"}]}},
            "status":{"stringValue":"waiting"},
            "created_at":{"timestampValue":"%s"}
          }}|}
        game_id
        capacity
        first_player
        ((new%js Js.date_now)##toISOString |> Js.to_string)
    ;;

    let encode_players players =
      let vals =
        players
        |> List.map ~f:(fun p -> Printf.sprintf {|{"stringValue":"%s"}|} p)
        |> String.concat ~sep:","
      in
      Printf.sprintf {|{"fields":{"players":{"arrayValue":{"values":[%s]}}}}|} vals
    ;;

    let decode ~game_id json =
      let open Json in
      let fields = get json "fields" in
      let capacity =
        match opt fields with
        | false -> 0
        | true ->
          let v = get (get fields "capacity") "integerValue" in
          int_of_jsint v
      in
      let status =
        match opt fields with
        | false -> "waiting"
        | true ->
          let v = get (get fields "status") "stringValue" in
          to_string_exn v
      in
      let players =
        match opt fields with
        | false -> []
        | true ->
          let arr = get (get (get fields "players") "arrayValue") "values" in
          let n = arr##.length in
          let rec loop i acc =
            match Int.(i >= n) with
            | true -> List.rev acc
            | false ->
              let v = Js.Optdef.get (Js.array_get arr i) (Js.Unsafe.obj [||]) in
              let s = get v "stringValue" |> to_string_exn in
              loop (i + 1) (s :: acc)
          in
          loop 0 []
      in
      { game_id; capacity; players; status }
    ;;

    let run_query_first_waiting ~(capacity : int) =
      let where =
        Printf.sprintf
          {|{
              "structuredQuery":{
                "from":[{"collectionId":"lobby"}],
                "where":{"compositeFilter":{"op":"AND","filters":[
                  {"fieldFilter":{"field":{"fieldPath":"capacity"},"op":"EQUAL","value":{"integerValue":"%d"}}},
                  {"fieldFilter":{"field":{"fieldPath":"status"},"op":"EQUAL","value":{"stringValue":"waiting"}}}
                ]}},
                "limit":1
              }
            }|}
          capacity
      in
      let%bind.Deferred status, resp =
        Http.request ~meth:`POST ~url:Config.run_query ~body:where ()
      in
      match status with
      | s when s >= 200 && s < 300 ->
        let arr = Js.Unsafe.global##._JSON##parse (Js.string resp) in
        let first = Js.array_get arr 0 in
        let has_doc = Json.opt (Json.get first "document") in
        (match has_doc with
         | false -> Async_kernel.return None
         | true ->
           let doc = Json.get first "document" in
           let name = Json.get doc "name" |> Js.to_string in
           let game_id =
             match String.rsplit2 ~on:'/' name with
             | None -> name
             | Some (_p, id) -> id
           in
           let lobby = decode ~game_id doc in
           Async_kernel.return (Some lobby))
      | _ -> Async_kernel.return None
    ;;

    let get ~(game_id : string) =
      let%bind.Deferred status, resp =
        Http.request ~meth:`GET ~url:(Config.lobby_doc game_id) ()
      in
      match status with
      | s when s >= 200 && s < 300 ->
        let json = Js.Unsafe.global##._JSON##parse (Js.string resp) in
        Async_kernel.return (Ok (decode ~game_id json))
      | 404 -> Async_kernel.return (Error `Not_found)
      | _ -> Async_kernel.return (Error `Http)
    ;;

    let create ~(game_id : string) ~(capacity : int) ~(first_player : string) =
      let body = encode_create ~game_id ~capacity ~first_player in
      let%bind.Deferred status, _resp =
        Http.request ~meth:`PATCH ~url:(Config.lobby_upsert game_id) ~body ()
      in
      match status with
      | s when s >= 200 && s < 300 -> get ~game_id
      | _ -> Async_kernel.return (Error `Http)
    ;;

    let patch_players ~(game_id : string) ~(players : string list) =
      let body = encode_players players in
      let url = Config.lobby_doc game_id ^ "&updateMask.fieldPaths=players" in
      let%bind.Deferred status, _resp = Http.request ~meth:`PATCH ~url ~body () in
      match status with
      | s when s >= 200 && s < 300 -> get ~game_id
      | _ -> Async_kernel.return (Error `Http)
    ;;

    let set_started ~(game_id : string) =
      let body = {|{"fields":{"status":{"stringValue":"started"}}}|} in
      let url = Config.lobby_doc game_id ^ "&updateMask.fieldPaths=status" in
      let%bind.Deferred status, _resp = Http.request ~meth:`PATCH ~url ~body () in
      match status with
      | s when s >= 200 && s < 300 -> Async_kernel.return (Ok ())
      | _ -> Async_kernel.return (Error ())
    ;;
  end

  module Game_doc = struct
    type t =
      { game_id : string
      ; state : Game_state.t option
      }

    let encode_state (st : Game_state.t) =
      let s = Game_state.sexp_of_t st |> Sexp.to_string |> String.escaped in
      Printf.sprintf {|{"fields":{"state":{"stringValue":"%s"}}}|} s
    ;;

    let encode_create ~(game_id : string) ~(initial : Game_state.t) =
      let s = Game_state.sexp_of_t initial |> Sexp.to_string |> String.escaped in
      Printf.sprintf
        {|{"fields":{
            "game_id":{"stringValue":"%s"},
            "state":{"stringValue":"%s"},
            "created_at":{"timestampValue":"%s"}
          }}|}
        game_id
        s
        ((new%js Js.date_now)##toISOString |> Js.to_string)
    ;;

    let decode ~(game_id : string) json =
      let fields = Js.Unsafe.get json "fields" in
      let has_state = Json.opt (Js.Unsafe.get fields "state") in
      match has_state with
      | false -> { game_id; state = None }
      | true ->
        let s =
          Js.Unsafe.get (Js.Unsafe.get fields "state") "stringValue" |> Js.to_string
        in
        let sexp = Sexp.of_string s in
        let st = Game_state.t_of_sexp sexp in
        { game_id; state = Some st }
    ;;

    let create ~(game_id : string) ~(initial : Game_state.t) =
      let body = encode_create ~game_id ~initial in
      let%bind.Deferred status, _resp =
        Http.request ~meth:`PATCH ~url:(Config.game_upsert game_id) ~body ()
      in
      match status with
      | s when s >= 200 && s < 300 -> Async_kernel.return (Ok ())
      | 409 -> Async_kernel.return (Ok ())
      | _ -> Async_kernel.return (Error ())
    ;;

    let get ~(game_id : string) =
      let%bind.Deferred status, resp =
        Http.request ~meth:`GET ~url:(Config.game_doc game_id) ()
      in
      match status with
      | s when s >= 200 && s < 300 ->
        let json = Js.Unsafe.global##._JSON##parse (Js.string resp) in
        Async_kernel.return (Ok (decode ~game_id json))
      | 404 -> Async_kernel.return (Ok { game_id; state = None })
      | _ -> Async_kernel.return (Error ())
    ;;

    let save ~(game_id : string) ~(state : Game_state.t) =
      let body = encode_state state in
      let%bind.Deferred status, _resp =
        Http.request ~meth:`PATCH ~url:(Config.game_doc game_id) ~body ()
      in
      match status with
      | s when s >= 200 && s < 300 -> Async_kernel.return (Ok ())
      | _ -> Async_kernel.return (Error ())
    ;;
  end
end

module Quickplay = struct
  let color_of_index i =
    match i with
    | 0 -> Player_kind.A
    | 1 -> Player_kind.B
    | 2 -> Player_kind.C
    | 3 -> Player_kind.D
    | 4 -> Player_kind.E
    | _ -> Player_kind.F
  ;;

  let initial_state ~capacity =
    match Game_state.create ~number_of_players:capacity with
    | Ok st -> st
    | Error _ -> failwith "init"
  ;;

  let join_or_create ~(capacity : int) =
    let open Async_kernel in
    let%bind found = Firebase.Lobby.run_query_first_waiting ~capacity in
    match found with
    | None ->
      let game_id = Id.generate () in
      let first = Player_label.make 0 in
      let%bind created = Firebase.Lobby.create ~game_id ~capacity ~first_player:first in
      (match created with
       | Error _ -> return (Error `Http)
       | Ok l ->
         return
           (Ok
              ( Firebase.Lobby.
                  { game_id = l.game_id
                  ; capacity = l.capacity
                  ; players = l.players
                  ; status = l.status
                  }
              , 0 )))
    | Some l ->
      let me_idx = List.length l.players in
      let label = Player_label.make me_idx in
      let players = l.players @ [ label ] in
      let%bind patched = Firebase.Lobby.patch_players ~game_id:l.game_id ~players in
      (match patched with
       | Error _ -> return (Error `Http)
       | Ok l' -> return (Ok (l', me_idx)))
  ;;

  let start_if_full (l : Firebase.Lobby.t) =
    let open Async_kernel in
    match
      Int.equal (List.length l.players) l.capacity, String.equal l.status "waiting"
    with
    | true, true ->
      let st0 = initial_state ~capacity:l.capacity in
      let%bind _ = Firebase.Game_doc.create ~game_id:l.game_id ~initial:st0 in
      let%bind _ = Firebase.Lobby.set_started ~game_id:l.game_id in
      return ()
    | _ -> return ()
  ;;
end

module Ui = struct
  module Screen = struct
    type t =
      | Landing
      | Waiting of
          { lobby : Firebase.Lobby.t
          ; me_index : int
          }
      | Playing_online of
          { game_id : string
          ; me_color : Player_kind.t
          ; state : Game_state.t
          }
      | Playing_offline of Game_state.t
    [@@deriving sexp, equal]
  end

  module Online = struct
    type t =
      { game_id : string
      ; me_index : int
      ; capacity : int
      ; me_color : Player_kind.t
      }
    [@@deriving sexp, equal]
  end

  module Model = struct
    type t =
      { screen : Screen.t
      ; path : Cell_position.t list
      ; online : Online.t option
      ; pending_save : Game_state.t option
      }
    [@@deriving sexp, equal]
  end

  let initial_model =
    { Model.screen = Landing; path = []; online = None; pending_save = None }
  ;;

  module Action = struct
    type t =
      | Start_game of int
      | Select_start of Cell_position.t
      | Extend_path of Cell_position.t
      | Confirm_path
      | Cancel_path
      | Lobby_refreshed of Firebase.Lobby.t
      | Game_refreshed of Game_state.t option
      | Clear_pending_save
  end

  let pos_equal a b = Int.equal (Cell_position.compare a b) 0

  let class_of_player = function
    | Player_kind.A -> "A"
    | B -> "B"
    | C -> "C"
    | D -> "D"
    | E -> "E"
    | F -> "F"
  ;;

  let color_name_of_player = function
    | Player_kind.A -> "Blue"
    | B -> "Orange"
    | C -> "Green"
    | D -> "Black"
    | E -> "Purple"
    | F -> "Red"
  ;;

  let apply_action (m : Model.t) (a : Action.t) : Model.t =
    match a, m.screen, m.online with
    | Start_game _, _, _ -> m
    | Cancel_path, Playing_offline _, _ -> { m with path = [] }
    | Cancel_path, Playing_online _, _ -> { m with path = [] }
    | Cancel_path, _, _ -> m
    | Select_start pos, Playing_offline st, _ ->
      (match st.decision with
       | Decision.In_progress { whose_turn } ->
         (match Map.find st.board pos with
          | Some (Some who) when Player_kind.equal who whose_turn && List.is_empty m.path
            -> { m with path = [ pos ] }
          | _ -> m)
       | _ -> m)
    | Select_start pos, Playing_online { state; _ }, Some online ->
      (match state.decision with
       | Decision.In_progress { whose_turn } ->
         (match Map.find state.board pos with
          | Some (Some who)
            when Player_kind.equal who whose_turn
                 && Player_kind.equal who online.me_color
                 && List.is_empty m.path -> { m with path = [ pos ] }
          | _ -> m)
       | _ -> m)
    | Select_start _, _, _ -> m
    | Extend_path dst, Playing_offline st, _ ->
      (match List.is_empty m.path with
       | true -> m
       | false ->
         let nexts = Game_state.next_steps_from_path st ~path:m.path in
         (match List.mem nexts dst ~equal:pos_equal with
          | true -> { m with path = m.path @ [ dst ] }
          | false -> m))
    | Extend_path dst, Playing_online { state; _ }, _ ->
      (match List.is_empty m.path with
       | true -> m
       | false ->
         let nexts = Game_state.next_steps_from_path state ~path:m.path in
         (match List.mem nexts dst ~equal:pos_equal with
          | true -> { m with path = m.path @ [ dst ] }
          | false -> m))
    | Extend_path _, _, _ -> m
    | Confirm_path, Playing_offline st, _ ->
      (match List.length m.path >= 2 with
       | false -> m
       | true ->
         (match Game_state.is_move_valid st m.path with
          | Error _ -> { m with path = [] }
          | Ok () ->
            (match Game_state.make_move st m.path with
             | Ok st' -> { m with screen = Playing_offline st'; path = [] }
             | Error _ -> { m with path = [] })))
    | Confirm_path, Playing_online { state; game_id; _ }, Some online ->
      (match List.length m.path >= 2 with
       | false -> m
       | true ->
         (match Game_state.is_move_valid state m.path with
          | Error _ -> { m with path = [] }
          | Ok () ->
            (match Game_state.make_move state m.path with
             | Ok st' ->
               { m with
                 screen =
                   Playing_online { game_id; me_color = online.me_color; state = st' }
               ; path = []
               ; pending_save = Some st'
               }
             | Error _ -> { m with path = [] })))
    | Confirm_path, _, _ -> m
    | Lobby_refreshed l, Waiting _, _ ->
      (* no blocking fetch here; poll loop will fetch game state when started *)
      { m with
        screen =
          Waiting
            { lobby = l
            ; me_index = Option.value_exn (Option.map m.online ~f:(fun o -> o.me_index))
            }
      }
    | Lobby_refreshed _, _, _ -> m
    | Game_refreshed None, Playing_online _, _ -> m
    | Game_refreshed (Some st), Playing_online cur, _ ->
      (match Game_state.equal st cur.state with
       | true -> m
       | false -> { m with screen = Playing_online { cur with state = st } })
    | Game_refreshed _, _, _ -> m
    | Clear_pending_save, _, _ -> { m with pending_save = None }
  ;;

  let landing ~(inject : Action.t -> unit Ui_effect.t) =
    let cell lbl n extra =
      Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.classes ("landing__cell" :: extra)
          ; Vdom.Attr.on_click (fun _ -> inject (Action.Start_game n))
          ]
        [ Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "landing__label" ]
            [ Vdom.Node.text lbl ]
        ]
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "landing" ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "landing__title" ]
          [ Vdom.Node.text "How many players?" ]
      ; Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "landing__grid" ]
          [ cell "2" 2 [ "landing__cell--left"; "landing__cell--top" ]
          ; cell "3" 3 [ "landing__cell--top" ]
          ; cell "4" 4 [ "landing__cell--left" ]
          ; cell "6" 6 []
          ]
      ]
  ;;

  let render_board
        ~(st : Game_state.t)
        ~(path : Cell_position.t list)
        ~(inject : Action.t -> unit Ui_effect.t)
        ~(restrict_to : Player_kind.t option)
    =
    let cells = Map.to_alist st.board in
    let bounds = Layout.uniform_bounds cells in
    let fit = 0.92 in
    let cell_scale = 0.74 in
    let cell_w, cell_h = Layout.cell_size_pct ~bounds ~fit ~cell_scale in
    let next_steps = Game_state.next_steps_from_path st ~path in
    let is_next pos = List.mem next_steps pos ~equal:pos_equal in
    let moving_owner, moving_head =
      match path with
      | start :: _ ->
        (match Map.find st.board start with
         | Some (Some who) -> Some who, List.last path
         | _ -> None, None)
      | [] -> None, None
    in
    let can_confirm =
      match List.length path >= 2 with
      | false -> false
      | true ->
        (match Game_state.is_move_valid st path with
         | Ok () -> true
         | Error _ -> false)
    in
    let turn_class =
      match st.decision with
      | Decision.In_progress { whose_turn } -> class_of_player whose_turn
      | Decision.Winner _ -> "none"
    in
    let render_cell ((pos : Cell_position.t), occ) =
      let x, y = Layout.xy_of_axial (pos.q_coordinate, pos.r_coordinate) in
      let left_pct, top_pct = Layout.normalize ~bounds ~fit (x, y) in
      let alt = (pos.q_coordinate + pos.r_coordinate) land 1 = 0 in
      let is_path_start =
        match path with
        | s :: _ -> pos_equal s pos
        | _ -> false
      in
      let base_piece =
        match occ with
        | None -> Vdom.Node.none
        | Some who ->
          let selectable =
            match st.decision, path, restrict_to with
            | Decision.In_progress { whose_turn }, [], None
              when Player_kind.equal whose_turn who ->
              [ Vdom.Attr.on_click (fun _ -> inject (Action.Select_start pos)) ]
            | Decision.In_progress { whose_turn }, [], Some mine
              when Player_kind.equal whose_turn who && Player_kind.equal who mine ->
              [ Vdom.Attr.on_click (fun _ -> inject (Action.Select_start pos)) ]
            | _ -> []
          in
          (match is_path_start with
           | true -> Vdom.Node.none
           | false ->
             Vdom.Node.div
               ~attrs:
                 (Vdom.Attr.classes [ "piece"; "piece--" ^ class_of_player who ]
                  :: selectable)
               [])
      in
      let moving_piece =
        match moving_owner, moving_head with
        | Some who, Some head when pos_equal pos head ->
          Vdom.Node.div
            ~attrs:
              [ Vdom.Attr.classes
                  [ "piece"; "piece--moving"; "piece--" ^ class_of_player who ]
              ]
            []
        | _ -> Vdom.Node.none
      in
      let dest_click_attr =
        match is_next pos with
        | true -> Vdom.Attr.on_click (fun _ -> inject (Action.Extend_path pos))
        | false -> Vdom.Attr.empty
      in
      let classes =
        [ "cell" ]
        @ (match alt with
           | true -> [ "cell--alt" ]
           | false -> [])
        @
        match is_next pos with
        | true -> [ "cell--next" ]
        | false -> []
      in
      let style =
        Css_gen.(
          left (`Percent (Percent.of_percentage left_pct))
          @> top (`Percent (Percent.of_percentage top_pct))
          @> width (`Percent (Percent.of_percentage cell_w))
          @> height (`Percent (Percent.of_percentage cell_h)))
      in
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes classes; Vdom.Attr.style style; dest_click_attr ]
        [ base_piece; moving_piece ]
    in
    let hud =
      match path with
      | [] -> Vdom.Node.none
      | _ :: _ ->
        Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "hud" ]
          [ Vdom.Node.div
              ~attrs:[ Vdom.Attr.class_ "hud__actions" ]
              [ Vdom.Node.button
                  ~attrs:
                    ([ Vdom.Attr.class_ "btn btn--confirm" ]
                     @
                     match can_confirm with
                     | true ->
                       [ Vdom.Attr.on_click (fun _ -> inject Action.Confirm_path) ]
                     | false -> [ Vdom.Attr.create "disabled" "" ])
                  [ Vdom.Node.text "Confirm" ]
              ; Vdom.Node.button
                  ~attrs:
                    [ Vdom.Attr.class_ "btn btn--cancel"
                    ; Vdom.Attr.on_click (fun _ -> inject Action.Cancel_path)
                    ]
                  [ Vdom.Node.text "Cancel" ]
              ]
          ]
    in
    let win_banner =
      match st.decision with
      | Decision.Winner who ->
        let who_name = color_name_of_player who in
        Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ ("banner banner--" ^ class_of_player who) ]
          [ Vdom.Node.text (who_name ^ " Wins!") ]
      | _ -> Vdom.Node.none
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.classes [ "game"; "game--turn-" ^ turn_class ] ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.classes [ "board"; "board--turn-" ^ turn_class ] ]
          (List.map cells ~f:render_cell)
      ; hud
      ; win_banner
      ]
  ;;

  let waiting_view ~(lobby : Firebase.Lobby.t) ~me_index =
    let who =
      match List.nth lobby.players me_index with
      | None -> "You"
      | Some s -> s
    in
    Vdom.Node.div
      [ Vdom.Node.h2 [ Vdom.Node.text "Quick Match" ]
      ; Vdom.Node.div [ Vdom.Node.text (Printf.sprintf "Game ID: %s" lobby.game_id) ]
      ; Vdom.Node.div
          [ Vdom.Node.text
              (Printf.sprintf "Players: %d/%d" (List.length lobby.players) lobby.capacity)
          ]
      ; Vdom.Node.ul
          (List.map lobby.players ~f:(fun p -> Vdom.Node.li [ Vdom.Node.text p ]))
      ; Vdom.Node.div [ Vdom.Node.text (Printf.sprintf "You are %s" who) ]
      ; Vdom.Node.div [ Vdom.Node.text "Waiting for opponents..." ]
      ]
  ;;

  let view (m : Model.t) ~(inject : Action.t -> unit Ui_effect.t) =
    match m.screen with
    | Landing -> landing ~inject
    | Waiting { lobby; me_index } -> waiting_view ~lobby ~me_index
    | Playing_offline st -> render_board ~st ~path:m.path ~inject ~restrict_to:None
    | Playing_online { state; me_color; _ } ->
      render_board ~st:state ~path:m.path ~inject ~restrict_to:(Some me_color)
  ;;

  let component () =
    let%sub model, set_model = Bonsai.state (module Model) ~default_model:initial_model in
    let%sub on_start_click =
      let%arr set_model = set_model
      and model = model in
      fun players ->
        let open Vdom.Effect.Let_syntax in
        let%bind res =
          Bonsai_web.Effect.of_deferred_fun
            (fun n -> Quickplay.join_or_create ~capacity:n)
            players
        in
        match res with
        | Error _ -> Vdom.Effect.Ignore
        | Ok (lobby, me_index) ->
          let me_color = Quickplay.color_of_index me_index in
          let online =
            Online.
              { game_id = lobby.game_id; me_index; capacity = lobby.capacity; me_color }
          in
          let m' =
            { model with
              screen = Screen.Waiting { lobby; me_index }
            ; online = Some online
            ; path = []
            }
          in
          set_model m'
    in
    let%sub poll_lobby =
      let%arr model = model
      and set_model = set_model in
      match model.screen, model.online with
      | Waiting { lobby; _ }, _ ->
        let open Vdom.Effect.Let_syntax in
        let%bind res =
          Bonsai_web.Effect.of_deferred_fun
            (fun gid -> Firebase.Lobby.get ~game_id:gid)
            lobby.game_id
        in
        (match res with
         | Ok l' ->
           (* try to flip to started if capacity reached *)
           let%bind () =
             Bonsai_web.Effect.of_deferred_fun (fun l -> Quickplay.start_if_full l) l'
           in
           (* if lobby has started, fetch game state and transition; otherwise just refresh lobby *)
           if String.equal l'.status "started"
           then (
             let%bind gs =
               Bonsai_web.Effect.of_deferred_fun
                 (fun gid -> Firebase.Game_doc.get ~game_id:gid)
                 l'.game_id
             in
             match gs with
             | Ok { game_id = _; state = Some st } ->
               let online = Option.value_exn model.online in
               set_model
                 { model with
                   screen =
                     Playing_online
                       { game_id = l'.game_id; me_color = online.me_color; state = st }
                 }
             | _ ->
               (* game state not persisted yet; keep waiting UI updated *)
               set_model (apply_action model (Action.Lobby_refreshed l')))
           else set_model (apply_action model (Action.Lobby_refreshed l'))
         | Error _ -> Vdom.Effect.Ignore)
      | _, _ -> Vdom.Effect.Ignore
    in
    let%sub poll_game =
      let%arr model = model
      and set_model = set_model in
      match model.screen with
      | Playing_online { game_id; _ } ->
        let open Vdom.Effect.Let_syntax in
        let%bind res =
          Bonsai_web.Effect.of_deferred_fun
            (fun gid -> Firebase.Game_doc.get ~game_id:gid)
            game_id
        in
        (match res with
         | Ok { state = Some st; _ } ->
           set_model (apply_action model (Action.Game_refreshed (Some st)))
         | Ok { state = None; _ } -> Vdom.Effect.Ignore
         | Error _ -> Vdom.Effect.Ignore)
      | _ -> Vdom.Effect.Ignore
    in
    let%sub flush_pending_save =
      let%arr model = model
      and set_model = set_model in
      match model.pending_save, model.online with
      | Some st, Some online ->
        let open Vdom.Effect.Let_syntax in
        let%bind _ =
          Bonsai_web.Effect.of_deferred_fun
            (fun (gid, st) -> Firebase.Game_doc.save ~game_id:gid ~state:st)
            (online.game_id, st)
        in
        set_model (apply_action model Action.Clear_pending_save)
      | _ -> Vdom.Effect.Ignore
    in
    let%sub () =
      let%sub () =
        Bonsai.Clock.every
          ~when_to_start_next_effect:`Every_multiple_of_period_blocking
          (Time_ns.Span.of_sec 0.8)
          poll_lobby
      in
      Bonsai.const ()
    in
    let%sub () =
      let%sub () =
        Bonsai.Clock.every
          ~when_to_start_next_effect:`Every_multiple_of_period_blocking
          (Time_ns.Span.of_sec 0.5)
          poll_game
      in
      Bonsai.const ()
    in
    let%sub () =
      let%sub () =
        Bonsai.Clock.every
          ~when_to_start_next_effect:`Every_multiple_of_period_blocking
          (Time_ns.Span.of_sec 0.2)
          flush_pending_save
      in
      Bonsai.const ()
    in
    let%arr model = model
    and set_model = set_model
    and on_start_click = on_start_click in
    let inject (a : Action.t) =
      match a with
      | Action.Start_game n -> on_start_click n
      | _ -> set_model (apply_action model a)
    in
    view model ~inject
  ;;
end

let app = Ui.component ()
let () = Bonsai_web.Start.start app
