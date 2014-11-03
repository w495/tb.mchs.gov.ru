%%% @file dao_bGame.erl
%%%
%%%     Администрирование флеш-игры.
%%%

-module(dao_doc).
-compile(export_all).

-include("../../include/common.hrl").

% делаем доки из директорий
iter_store_dir([Dir|T], Ret) ->
    io:format("store dir:~p...~n", [Dir]),
    Id              = proplists:get_value("id"),
    Name            = proplists:get_value("name"),
    Parent_dir_id   = proplists:get_value("parent_dir_id"),
    Datatime        = proplists:get_value("datatime"),
    Doc_description_id = proplists:get_value("doc_description_id"),
    Dir_type_name   = proplists:get_value("dir_type_name"),
    Doc_content = proplists:get_value("doc_content"),
    Pic_url = proplists:get_value("pic_url"),
    Pdf_url = proplists:get_value("pdf_url"),
    Msw_url = proplists:get_value("msw_url"),


    if
        Id =:= 3 -> % Нормативные акты
            Type_name = "norm_acts";
        Id =:= 4 -> % Безопасность на транспорте
            Type_name = "transp_safety";

        true ->
            Type_name = Dir_type_name
    end,


    Q2 =    "insert into doc (name, content, datatime, doc_type_name, published, deleted, pic_url, pdf_url, msw_url, idx) values "
            "($1, $2, $3, $4, $5, $6, $7, $8, $9, $10) returning id",
    dao:withConnectionMchs(fun(Con) ->
        ok
    end),
    DocId = 1,
    % апдейтим проплист - вставляем ID нового док
    iter_store_dir(T, [[{"docid", DocId}|Dir]|Ret]);
iter_store_dir([], Ret) ->
    Ret.

convert() ->
    Q1 =    "select directory.*, dir_type.name as dir_type_name, document.content as doc_content, document.pic_url, document.pdf_url, "
            "document.msw_url "
            "from directory join dir_type on directory.dir_type_id = dir_type.id where deleted = false order by id desc;",
    {ok, Dirs} = dao:simple(Q1),
    file:write_file("/tmp/1", ?FMT("~p", [Dirs])),

    % делаем доки из директорий
    % связываем 
    io:format("~p~n", [Dirs]).

