-module(tic_tac_toe).

-export([new_game/0, 
		win/1, 
		move/3
		]).


new_game() ->
    {{f, f, f},
     {f, f, f},
     {f, f, f}}.


win(GameState) ->
    case GameState of
        {{Winner, Winner, Winner},
         {_, _, _},
         {_, _, _}} when Winner =:= x; Winner =:= o -> {win, Winner};
        {{_, _, _},
         {Winner, Winner, Winner},
         {_, _, _}} when Winner =:= x; Winner =:= o -> {win, Winner};
        {{_, _, _},
         {_, _, _},
         {Winner, Winner, Winner}} when Winner =:= x; Winner =:= o -> {win, Winner};
        {{Winner, _, _},
         {Winner, _, _},
         {Winner, _, _}} when Winner =:= x; Winner =:= o -> {win, Winner};
        {{_, Winner, _},
         {_, Winner, _},
         {_, Winner, _}} when Winner =:= x; Winner =:= o -> {win, Winner};
        {{_, _, Winner},
         {_, _, Winner},
         {_, _, Winner}} when Winner =:= x; Winner =:= o -> {win, Winner};
        {{Winner, _, _},
         {_, Winner, _},
         {_, _, Winner}} when Winner =:= x; Winner =:= o -> {win, Winner};
        {{_, _, Winner},
         {_, Winner, _},
         {Winner, _, _}} when Winner =:= x; Winner =:= o -> {win, Winner};
        {{_, _, _},
         {_, _, _},
         {_, _, _}} -> no_win
	end.


move(Cell, Player, GameState) ->
	if 
		Cell =:= 1 ->
			case GameState of
				{{Input, Value2, Value3},
				 {Value4, Value5, Value6},
				 {Value7, Value8, Value9}}  when Input =:= f -> 
                    {ok, {{Player, Value2, Value3},
                         {Value4, Value5, Value6},
                         {Value7, Value8, Value9}}};
				{{Input, _, _},
				 {_, _, _},
				 {_, _, _}}  when Input =/= f -> {error, invalid_move}
			end;
		Cell =:= 2 ->
			case GameState of
				{{Value1, Input, Value3},
				 {Value4, Value5, Value6},
				 {Value7, Value8, Value9}}  when Input =:= f -> 
                    {ok, {{Value1, Player, Value3},
                         {Value4, Value5, Value6},
                         {Value7, Value8, Value9}}};
				{{_, Input, _},
				 {_, _, _},
				 {_, _, _}}  when Input =/= f -> {error, invalid_move}
			end;
		Cell =:= 3 ->
			case GameState of
				{{Value1, Value2, Input},
				 {Value4, Value5, Value6},
				 {Value7, Value8, Value9}}  when Input =:= f -> 
                    {ok, {{Value1, Value2, Player},
                         {Value4, Value5, Value6},
                         {Value7, Value8, Value9}}};
				{{_, _, Input},
				 {_, _, _},
				 {_, _, _}}  when Input =/= f -> {error, invalid_move}
			end;
		Cell =:= 4 ->
			case GameState of
				{{Value1, Value2, Value3},
				 {Input, Value5, Value6},
				 {Value7, Value8, Value9}}  when Input =:= f -> 
                    {ok, {{Value1, Value2, Value3},
                         {Player, Value5, Value6},
                         {Value7, Value8, Value9}}};
				{{_, _, _},
				 {Input, _, _},
				 {_, _, _}}  when Input =/= f -> {error, invalid_move}
			end;
		Cell =:= 5 ->
			case GameState of
				{{Value1, Value2, Value3},
				 {Value4, Input, Value6},
				 {Value7, Value8, Value9}}  when Input =:= f -> 
                    {ok, {{Value1, Value2, Value3},
                         {Value4, Player, Value6},
                         {Value7, Value8, Value9}}};
				{{_, _, _},
				 {_, Input, _},
				 {_, _, _}}  when Input =/= f -> {error, invalid_move}
			end;
		Cell =:= 6 ->
			case GameState of
				{{Value1, Value2, Value3},
				 {Value4, Value5, Input},
				 {Value7, Value8, Value9}}  when Input =:= f -> 
                    {ok, {{Value1, Value2, Value3},
                         {Value4, Value5, Player},
                         {Value7, Value8, Value9}}};
				{{_, _, _},
				 {_, _, Input},
				 {_, _, _}}  when Input =/= f -> {error, invalid_move}
			end;
		Cell =:= 7 ->
			case GameState of
				{{Value1, Value2, Value3},
				 {Value4, Value5, Value6},
				 {Input, Value8, Value9}}  when Input =:= f -> 
                    {ok, {{Value1, Value2, Value3},
                         {Value4, Value5, Value6},
                         {Player, Value8, Value9}}};
				{{_, _, _},
				 {_, _, _},
				 {Input, _, _}}  when Input =/= f -> {error, invalid_move}
			end;
		Cell =:= 8 ->
			case GameState of
				{{Value1, Value2, Value3},
				 {Value4, Value5, Value6},
				 {Value7, Input, Value9}}  when Input =:= f -> 
                    {ok, {{Value1, Value2, Value3},
                         {Value4, Value5, Value6},
                         {Value7, Player, Value9}}};
				{{_, _, _},
				 {_, _, _},
				 {_, Input, _}}  when Input =/= f -> {error, invalid_move}
			end;
		Cell =:= 9 ->
			case GameState of
				{{Value1, Value2, Value3},
				 {Value4, Value5, Value6},
				 {Value7, Value8, Input}}  when Input =:= f -> 
                    {ok, {{Value1, Value2, Value3},
                         {Value4, Value5, Value6},
                         {Value7, Value8, Player}}};
				{{_, _, _},
				 {_, _, _},
				 {_, _, Input}}  when Input =/= f -> {error, invalid_move}
			end;
		true -> {error, invalid_move}
    end.





			%case GameState of
				%{{Input, Value, Value},
				 %{Value, Value, Value},
				 %{Value, Value, Value}} -> 
					%if 
						%Input =:= f -> 
							%{{Player, Value, Value},
							 %{Value, Value, Value},
							 %{Value, Value, Value}};
						%true -> {error, invalid_move}
					%end;
