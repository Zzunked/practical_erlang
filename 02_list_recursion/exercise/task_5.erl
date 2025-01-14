-module(task_5).

-export([splitwith/2, zipwith/3]).

-include_lib("eunit/include/eunit.hrl").


%% implement lists:splitwith/2
%% http://www.erlang.org/doc/man/lists.html#splitwith-2
splitwith(Pred, List) ->
    {task_4:takewhile(Pred, List), task_4:dropwhile(Pred, List)}.


splitwith_test() ->
    F = fun(Val) -> Val rem 2 =:= 0 end,
    ?assertEqual({[], []}, splitwith(F, [])),
    ?assertEqual({[], [1]}, splitwith(F, [1])),
    ?assertEqual({[], [1,2]}, splitwith(F, [1,2])),
    ?assertEqual({[], [1,2,3]}, splitwith(F, [1,2,3])),
    ?assertEqual({[4,6,8], [1,2,3]}, splitwith(F, [4,6,8,1,2,3])),
    ?assertEqual({[2], [3,4,5]}, splitwith(F, [2,3,4,5])),
    ok.


%% implement lists:zipwith/3
%% http://www.erlang.org/doc/man/lists.html#zipwith-3
%% if two lists have different lengths don't throw exception but ignore the rest of longer list
zipwith(Pred, List1, List2) -> zipwith(Pred, List1, List2, []).

zipwith(_, List1, [], Acc) -> task_2:reverse(Acc);
zipwith(_, [], List2, Acc) -> task_2:reverse(Acc);
zipwith(Pred, [H1|T1], [H2|T2], Acc) ->
    zipwith(Pred, T1, T2, [Pred(H1, H2)|Acc]).


zipwith_test() ->
    F = fun(Val1, Val2) -> Val1 + Val2 end,
    ?assertEqual([7,7,7], zipwith(F, [1,2,3], [6,5,4])),
    ?assertEqual([], zipwith(F, [], [])),
    ?assertEqual([4], zipwith(F, [1], [3])),
    ?assertEqual([4,6], zipwith(F, [1,2], [3,4])),
    ?assertEqual([2,4,6,8,10], zipwith(F, [1,2,3,4,5], [1,2,3,4,5])),
    ?assertEqual([2,4,6,8], zipwith(F, [1,2,3,4], [1,2,3,4,5,6])),
    ?assertEqual([2,4,6,8], zipwith(F, [1,2,3,4,5,6], [1,2,3,4])),
    ok.
