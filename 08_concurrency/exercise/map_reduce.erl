-module(map_reduce).

-export([
    start/1, reducer/2, mapper/2,
    analyze_file/1, aggregate/1, aggregate/2
]).


start(Files) ->
    %создать редьюсер процесс, который спавнит воркеров по количеству файлов
    %ожидает результаты
    %при получении их выдает
    RootPid = self(),
    NumWorkers = length(Files),
    ReducerPid = spawn(?MODULE, reducer, [RootPid, NumWorkers]),
    [spawn(?MODULE, mapper, [File, ReducerPid]) || File <- Files],
    receive
        {result, Result} -> Result
    after 
        2000 -> {error, no_reply}
    end.


reducer(RootPid, NumWorkers) ->
    %собирает згачения с каждого файла
    %делает свертку значений
    %отправляет их родительскому процессу
    DataList = [wait_for_data() || _ <- lists:seq(1, NumWorkers)],
    Result = aggregate(DataList),
    RootPid ! {result, Result},
    ok.


wait_for_data() ->
    %получает данные и проходит по ним мапой
    receive
        {mapper, Pid, File, Data} ->
            Data
    after 
        2000 -> #{}
    end.
        


mapper(File, ReducerPid) ->
    %получает файл прогоняет через аналайзер
    %результат своей работы отправляет редьюсеру
    Data = analyze_file(File),
    ReducerPid ! {mapper, self(), File, Data},
    ok.


analyze_file(File) ->
    %читает файл
    %если читается успешно, передает в аналайзер данных
    case file:read_file(File) of
        {ok, Data} -> analyze_data(Data);
        {error, _} -> #{}
    end.


analyze_data(Data) ->
    %разбивает текст по словам
    %делает свертку, собирает мапу,
    %где слова это ключи, а значения, количество вхождений
    Words = binary:split(Data, [<<" ">>, <<"\n">>], [global]),
    lists:foldl(
      fun (<<>>, Acc) -> Acc;
          (Word, Acc) ->
              case maps:find(Word, Acc) of
                  {ok, Counter} -> Acc#{Word => Counter + 1};
                  error -> Acc#{Word => 1}
              end
      end, #{}, Words).


aggregate(DataList) ->
    %собирает сверткой все данные в один список
    lists:foldl(
        fun(Data, Acc) -> aggregate(Data, Acc) end, 
        #{}, DataList).


aggregate(Data1, Data2) ->
    %проверяет наличие одинаковых ключей в двух мапах
    %если есть такие, суммирует их значение под одним ключем
    maps:fold(
        fun(Word, Counter, Acc) ->
                case maps:find(Word, Data2) of
                    {ok, Counter2} -> Acc#{Word => Counter + Counter2};
                    error -> Acc#{Word => Counter}
                end
        end, Data2, Data1).


