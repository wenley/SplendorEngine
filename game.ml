open Globals
open Data

type player = {
  name: string;
  tokens: tokens;
  reserved: card list;
  purchased: card list;
  nobles: noble list
}

type tier = {
  deck: card list;
  revealed: card list
}

type board = {
  one: tier;
  two: tier;
  three: tier;
  nobles: noble list;
  tokens: tokens
}

let start_board (num_players : int) : board =
  let color_token_count =
    match num_players with
    | 2 -> 4
    | 3 -> 5
    | 4 -> 7
    | _ -> failwith (Printf.sprintf "Unsupported number of players %d" num_players)
  in
  let tokens =
    TokenMap.empty |> TokenMap.add Normal(Black) color_token_count
                   |> TokenMap.add Normal(Blue) color_token_count
                   |> TokenMap.add Normal(Green) color_token_count
                   |> TokenMap.add Normal(Red) color_token_count
                   |> TokenMap.add Normal(White) color_token_count
                   |> TokenMap.add Gold 5
  in
  {
    one = {
      deck = []; revealed = []
    };
    two = {
      deck = []; revealed = []
    };
    three = {
      deck = []; revealed = []
    };
    nobles = [];
    tokens = tokens
  }

