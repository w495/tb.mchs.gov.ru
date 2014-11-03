%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_cfSecure).
-include("../include/web_session.hrl").
-include("../include/common.hrl").
-include("../include/dao.hrl").

-compile(export_all).

list(_Req, {_, Param, [SelfParent_, ItemParent | _ ],   [XslRoot | _ ]}) ->
    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/secure/list-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/secure/list-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/secure/list.xsl"
            end
    end,
    TargetId = utils:getTargetId(Param, ?SECURE_ID),
    
    {ok, Dir, Attaches} = dao_fDirectory:getDirA(TargetId),

    case Param of
        "" ->
            SelfParent = SelfParent_,
            IsRoot = true;
        _  ->
            [DirS] = Dir,    
            Parent_dir_id = proplists:get_value("parent_dir_id", DirS),
            SelfParent = string:join([SelfParent_,  utils:to_string(Parent_dir_id), "/" ], ""),
            IsRoot = false
    end,
    
    Attaches_ = [
        [ {"size", web_file:getAttachSizeString(Attach)} | Attach ]
        || Attach <- Attaches
    ],
    
    {ok, DirList} = dao_fDirectory:getDirs({TargetId}),
    {ok, DocList} = dao_fDirectory:getDocs({TargetId}),
    
    Meta = [[
            {"current-path", _Req:get(path)},
            {"parent-path", SelfParent},
            {"is-root", IsRoot},
            {"self-parent-path", SelfParent},
            {"item-parent-path", ItemParent},
            
            {"is-spec",             _Req:get_cookie_value(?SPEC)},
            {"spec-variant",        _Req:get_cookie_value(?VARIANT)},
            {"spec-color",          _Req:get_cookie_value(?COLOR)}
    ]],
    XMLTerms  = pg2xml:encodeData(
        [
            {one,  Dir,     "doc"},         % сама директория
            {list, DirList, "dirs"},        % список резделов
            {list, DocList, "docs"},        % список документов
            {list, Attaches_, "attaches"},   % список вложений
            {one, Meta, "meta"}             % описание запроса
        ]
    ),
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.
