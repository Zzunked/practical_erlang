-module(fizz_buzz).
-export([fizzbuzz/0]).


%% Реализовать Fizz Buzz
%% https://habr.com/ru/post/298134/

%Напишите программу, которая выводит на экран числа от 1 до 100. 
%При этом вместо чисел, кратных трем, программа должна выводить слово «Fizz», 
%а вместо чисел, кратных пяти — слово «Buzz». Если число кратно и 3, и 5, 
%то программа должна выводить слово «FizzBuzz»


substitude(ListOfNumbers) -> substitude(ListOfNumbers, []).

substitude([], Acc) -> lists:reverse(Acc);
substitude([Number|T], Acc) ->
    if 
        Number rem 3 =:= 0, Number rem 5 =/= 0 -> substitude(T, ["Fizz" | Acc]);
        Number rem 3 =/= 0, Number rem 5 =:= 0 -> substitude(T, ["Buzz" | Acc]);
        Number rem 3 =:= 0, Number rem 5 =:= 0 -> substitude(T, ["FizzBuzz" | Acc]);
        true -> substitude(T, [Number | Acc])
    end.

print_values_one_by_one([]) -> ok;
print_values_one_by_one([H|T]) ->
    io:format("~p, ", [H]),
    print_values_one_by_one(T).

fizzbuzz() ->
    ListOfNumbers = lists:seq(1, 100),
    FizzBuzzList = substitude(ListOfNumbers),
    print_values_one_by_one(FizzBuzzList).
