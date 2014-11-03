%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_file).

-import(mochiweb_cookies, [cookie/2]).
-include("../include/web_session.hrl").
-include("../include/common.hrl").

-include_lib("kernel/include/file.hrl").

-define(DIR, "/tmp/").

-define(SIZE_KiB, (1024)).
-define(SIZE_Bite_NAME, "Байт").
-define(SIZE_KiB_NAME,  "Кб").
-define(SIZE_MiB_NAME,  "Мб").
-define(SIZE_GiB_NAME,  "Гб").

-compile(export_all).

mkAttach(Filename, DirId, AttId, Content) ->
    FilePath = string:join(["static/docs", utils:to_list(DirId), utils:to_list(AttId), Filename], "/"),
    filelib:ensure_dir(FilePath),
    case file:open(FilePath, [raw,write]) of
        {ok, File} ->
            file:write(File, Content),
            file:close(File);
        {error, Error} ->
            ?ERROR(?FMT("Couldn't open ~p for writing, error: ~p~n", [FilePath, Error]))
    end.

chunk_handler(Filename, ContentType, TempFilename, File) ->
    fun(Next) ->
        case Next of
            eof ->
                file:close(File),
                {Filename, ContentType, TempFilename};
            Data ->
                file:write(File, Data),
                chunk_handler(Filename, ContentType, TempFilename, File)
        end
    end.

handle_file(Filename, ContentType) ->
    TempFilename = string:join(["/tmp/", atom_to_list(?MODULE), integer_to_list(erlang:phash2(make_ref()))], ""),
    
    {ok, File} = file:open(TempFilename, [raw, write]),
    chunk_handler(Filename, ContentType, TempFilename, File).


upload_answer_files([File| Files_tail] = Files, [File_type| File_type_tail] =  File_types, AnsId, UID) ->
    {OriginalFilename, _, TempFilename} =  File,
    
    case OriginalFilename of
        [] -> upload_answer_files(Files_tail, File_type_tail, AnsId, UID);
        _ ->
            case AnsId of
                undefined ->
                    DirId = string:join(["common", integer_to_list(erlang:phash2(make_ref()))], "/");
                _ ->
                    DirId = AnsId
            end,
            Destination = string:join(["static/data/docs", utils:to_list(DirId), OriginalFilename], "/"),
        %    filelib:ensure_dir(Destination),
        %    file:delete(Destination),
        %    file:copy(TempFilename, Destination),
            moveFile(TempFilename, Destination),
            
            dao_fDirectory:updateAttach({null, OriginalFilename, OriginalFilename, AnsId, File_type, Destination, UID}),
            upload_answer_files(Files_tail, File_type_tail, AnsId, UID)
    end;
    
upload_answer_files([], [], AnsId, UID)-> ok.

    
upload_customer_image(Req) ->
    FileHandler = fun(Filename, ContentType) -> handle_file(Filename, ContentType) end,
    Files = mochiweb_multipart:parse_form(Req, FileHandler),
    
    Id = proplists:get_value("id", Files),
    Prev = proplists:get_value("prev", Files),
    {OriginalFilename, _, TempFilename} = proplists:get_value("uploadfile", Files),
   
    case Id of
        undefined ->
            CustomerId = string:join(["common", integer_to_list(erlang:phash2(make_ref()))], "/");
        _ ->
            CustomerId = Id
    end,
    
    io:format("Prev = ~s~n", [Prev]),
    
    Destination = string:join(["static/data/users", utils:to_list(CustomerId), OriginalFilename], "/"),
%    filelib:ensure_dir(Destination),
%    file:delete(Destination),
%    case file:rename(TempFilename, Destination) of
    case moveFile(TempFilename, Destination) of
        ok ->
            case Prev of
                Destination -> ok;
                _ ->
                    file:delete(Prev)
            end,
            {"text/html", [], [Destination]};
        {error, Reason} ->
            file:delete(TempFilename),
            {"text/html", [], ["error"]}
    end.
    
    
upload_doc_image(Req) ->
    FileHandler = fun(Filename, ContentType) -> handle_file(Filename, ContentType) end,
    Files = mochiweb_multipart:parse_form(Req, FileHandler),
    
    Id = proplists:get_value("id", Files),
    Prev = proplists:get_value("prev", Files),
    {OriginalFilename, _, TempFilename} = proplists:get_value("uploadfile", Files),
   
    case Id of
        undefined ->
            DirId = string:join(["common", integer_to_list(erlang:phash2(make_ref()))], "/");
        _ ->
            DirId = Id
    end,
    
    Destination = string:join(["static/data/docs", utils:to_list(DirId), OriginalFilename], "/"),
%    filelib:ensure_dir(Destination),
%    file:delete(Destination),
%    case file:rename(TempFilename, Destination) of
    case moveFile(TempFilename, Destination) of
        ok ->
            case Prev of
                Destination -> ok;
                _ ->
                    file:delete(Prev)
                    %file:del_dir(filename:dirname(Prev)),
            end,
            {"text/html", [], [Destination]};
        {error, Reason} ->
            file:delete(TempFilename),
            {"text/html", [], ["error"]}
    end.

upload_doc_attach(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    io:format("upload_attach~n"),
    FileHandler = fun(Filename, ContentType) -> handle_file(Filename, ContentType) end,
    Files = mochiweb_multipart:parse_form(Req, FileHandler),
    
    Doc_id = proplists:get_value("doc_id", Files),
    Name = proplists:get_value("name", Files),
    Alt = proplists:get_value("alt", Files),
    
    Attach_type_id = proplists:get_value("attach_type_id", Files),

    io:format("Id = ~s Name = ~s  Alt = ~s Type = ~s ~n", [Doc_id, Name, Alt, Attach_type_id]),
    
    Prev = proplists:get_value("prev", Files),
    {OriginalFilename, _, TempFilename} = proplists:get_value("uploadfile", Files),
    case Doc_id of
        undefined ->
            DirId = string:join(["common", integer_to_list(erlang:phash2(make_ref()))], "/");
        _ ->
            DirId = Doc_id
    end,
    
    Destination = string:join(["static/data/docs", utils:to_list(DirId), "attaches" , OriginalFilename], "/"),
%    filelib:ensure_dir(Destination),
    io:format("Destination = ~s ~n", [Destination]),
    
%    case file:rename(TempFilename, Destination) of
    case moveFile(TempFilename, Destination) of
        ok ->
            file:delete(Prev),
            file:del_dir(filename:dirname(Prev)),
            {Return, NewId} = dao_bDirectory:updateAttach({null, Name, Alt, Doc_id, Attach_type_id, Destination, UID}),
            {"text/html", [], [utils:to_string(NewId)]};
        {error, Reason} ->
            file:delete(TempFilename),
            {"text/html", [], ["error"]}
    end.



delete_doc_attach(Req) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),
    Id = utils:to_integer(proplists:get_value("id", Data)),
    io:format("delete_attach"),
    Res = dao:daoCall(dao_bDirectory, deleteAttach_returningUrl, {Id, UID}),
    
    {"application/json", [], [mochijson2:encode(Res)]}.


upload_adv_com_image(Req) ->
    FileHandler = fun(Filename, ContentType) -> handle_file(Filename, ContentType) end,
    Files = mochiweb_multipart:parse_form(Req, FileHandler),
    
    Id = proplists:get_value("id", Files),
    Prev = proplists:get_value("prev", Files),
    {OriginalFilename, _, TempFilename} = proplists:get_value("uploadfile", Files),
   
    case Id of
        undefined ->
            DirId = string:join(["common", integer_to_list(erlang:phash2(make_ref()))], "/");
        _ ->
            DirId = Id
    end,
    
    Destination = string:join(["static/data/adv", utils:to_list(DirId), OriginalFilename], "/"),
%    filelib:ensure_dir(Destination),
%    file:delete(Destination),
    case moveFile(TempFilename, Destination) of
        ok -> 
            case Prev of
                Destination -> ok;
                _ ->
                    file:delete(Prev)
            end,
            {"text/html", [], [Destination]};
        {error, Reason} ->
            file:delete(TempFilename),
            {"text/html", [], ["error"]}
    end.


moveFile(Src, Dst) ->
    case Src =:= Dst of
        true -> ok;
        false ->
            filelib:ensure_dir(Dst),
            file:delete(Dst),
            case file:copy(Src, Dst) of
                {ok, _} -> 
                    file:delete(Src),
                    ok;
                ERROR ->
                    io:format("error reason: ~p~n", [ERROR]),
                    ERROR
            end
    end.



getAttachSizeString(Attach) ->
    Rawsize = getAttachSize(Attach),
    Size_KiB = Rawsize  div ?SIZE_KiB,
    Size_MiB = Size_KiB div ?SIZE_KiB,
    Size_GiB = Size_MiB div ?SIZE_KiB,
    
    io:format("~nSize_KiB = ~p~n", [Size_KiB]),
    io:format("~nSize_MiB = ~p~n", [Size_MiB]),
    io:format("~nSize_GiB = ~p~n", [Size_GiB]),
    if
        Size_KiB  =:= 0
            -> Size = utils:sformat("~p ~s", [Rawsize,  ?SIZE_Bite_NAME]);
        Size_MiB =:= 0
            -> Size = utils:sformat("~p ~s", [Size_KiB, ?SIZE_KiB_NAME]);
        Size_GiB =:= 0
            -> Size = utils:sformat("~p ~s", [Size_MiB, ?SIZE_MiB_NAME]);
        true
            -> Size = utils:sformat("~p ~s", [Size_GiB, ?SIZE_GiB_NAME])
    end,
    Size.

getAttachSize(Attach) ->
    Url  = proplists:get_value("url", Attach),
    case file:read_file_info(Url) of
        {ok, Read_file_info} ->
	    io:format("Read_file_info = ~p", [Read_file_info]),
	    #file_info{size = Size} = Read_file_info,
	    io:format("~nSize = ~p", [Size]);
	_ -> Size = 0
    end,
    Size.
