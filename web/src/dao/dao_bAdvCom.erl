%%% @file dao_bAdvCom.erl
%%%
%%%     Администрирование баннеров.
%%%

-module(dao_bAdvCom).
-compile(export_all).

getAdvComs(_) ->
    Q = "select ac.id, ac.name, ac.datestart, ac.datestop, bp.alias from adv_com ac join banner_place bp on bp.id = ac.banner_place_id;",
    dao:simple(Q).

getAdvCom(Id) ->
    Q = "select id, name, datestart, datestop, banner_place_id, url, ref from adv_com where id=$1;",
    dao:simple(Q, [Id]).

deleteAdvCom(Id) ->
    Q = "delete from adv_com where id=$1;",
    dao:simple(Q, [Id]).

getBannerPlaces(_) ->
    Q = "select id, alias from banner_place;",
    dao:simple(Q).

updateAdvCom({null, Name, Ref, Datestart, Datestop, BannerPlaceId, Pic_url}) ->
    Q1 = "insert into adv_com (name, datestart, datestop, banner_place_id, ref) values ($1, $2, $3, $4, $5) returning id;",
    Q2 = "update adv_com set url=$1 where id=$2;",
    Ret = dao:withTransactionMchs(fun(Con) ->
        {ok, 1, _, [{AdvComId}]} = pgsql:equery(Con, Q1, [Name, Datestart, Datestop, BannerPlaceId, Ref]),
        OriginalFilename = filename:basename(Pic_url),
        Destination = string:join(["static/data/adv", utils:to_list(AdvComId), OriginalFilename], "/"),
        case utils:moveFile(Pic_url, Destination) of
            ok -> 
                pgsql:equery(Con, Q2, [Destination, AdvComId]);
            {error, Reason} -> 
                io:format("updateAdvCom can't move file (~p): ~p to ~p~n", [Reason, Pic_url, Destination]),
                file:delete(Pic_url)
        end,
        ok 
    end),
    dao:processPGRet2(Ret);

updateAdvCom({Id, Name, Ref, Datestart, Datestop, BannerPlaceId, Pic_url}) ->
    Q = "update adv_com set name=$1, ref=$2, datestart=$3, datestop=$4, banner_place_id=$5, url=$6 where id=$7;",
    Ret = dao:withTransactionMchs(fun(Con) ->
        OriginalFilename = filename:basename(Pic_url),
        Destination = string:join(["static/data/adv", utils:to_list(Id), OriginalFilename], "/"),
        {ok,1} = pgsql:equery(Con, Q, [Name, Ref, Datestart, Datestop, BannerPlaceId, Destination, Id]),
        case utils:moveFile(Pic_url, Destination) of
            ok -> ok;
            {error, Reason} -> 
                io:format("updateAdvCom can't move file (~p): ~p to ~p~n", [Reason, Pic_url, Destination]),
                file:delete(Pic_url)
        end,
        ok 
    end),
    dao:processPGRet2(Ret).


collectUniqAdvByPlace_iter([H|T], Ret) ->
    BP = proplists:get_value("banner_place", H),
    case proplists:get_value(BP, Ret, null) of
        null -> NRet = [{BP, [H]} | Ret];
        Val -> NRet = [{BP, [H|Val]} | proplists:delete(BP, Ret)]
    end,
    collectUniqAdvByPlace_iter(T, NRet);
collectUniqAdvByPlace_iter([], Ret) ->
    Ret.

getRandomAdv(Vals) ->
    Avds = collectUniqAdvByPlace_iter(Vals, []),
    %io:format("ADVS: ~p~n",[Avds]),
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    [lists:nth(random:uniform(length(X)), X) || {_, X} <- Avds].

getBanners() ->
    Q = "select ac.*, bp.name as banner_place "
    "from adv_com ac join banner_place bp on ac.banner_place_id=bp.id where now() > datestart and datestop > now();",
    case dao:simple(Q) of
        {ok, Vals} -> {ok, getRandomAdv(Vals)};
        E -> E
    end.
