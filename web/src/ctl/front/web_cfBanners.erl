%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_cfBanners).
-compile(export_all).

-import(mochiweb_cookies, [cookie/2, cookie/3]).

-include("../include/web_session.hrl").
-include("../include/common.hrl").
-include("../include/dao.hrl").



%% Разрешенные действия:
%%
%% index/1, index/2,
%% list/1, list/2,
%% details/1, details/2,
%% edit/1, edit/2
    
details(Req, {ajax, _ , Links_Options, Template_Options}) ->
    try
        Banners = dao:daoCall(dao_bAdvCom, getBanners, undefined, values),
        throw({ok, {banners_ajax, Banners}})
    catch
        throw:{ok, Ret} ->
            io:format("throw:{ok, Ret} = ~p", [Ret]),
            
            throw(Ret);
        throw:Error ->
            throw({banners_ajax, {error, Error}});
        _:Error->
            throw({banners_ajax, {error, Error}})
    end.
