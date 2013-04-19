%% Forwards offline messages to the push server.
%%
%% Since we host the ejabberd and push servers in the same
%% AWS availability zone, we assume the traffic is secure
%% (as per the AWS Security Whitepaper).
%%
%% There is therefore no need to endure the overhead of
%% establishing an SSL connection.
%%
%% Depends on the mochiweb_util module for url encoding.


-module(mod_push).

-vsn("0.1").

-behaviour(gen_mod).

-export([start/2, stop/1]).

%% callbacks
-export([on_set_presence/4, on_unset_presence/4]).
-export([on_offline_message/3, on_packet_send/3]).
-export([on_remove_user/2]).

%% public api
-export([set_presence/2, unset_presence/1]).
-export([offline_message/3, packet_send/2]).
-export([remove_user/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-record(background_counter, {usr :: string(), %% username@example.com/Resource
                             count = 0 :: integer(),
                             status = online :: atom() %% online, background, offline
                            }).

-define(DBT, mod_push_background).
-define(XMLNS, "http://adeimantus.com").

-define(MAX_RETRIES, 4).

%%%===================================================================
%%% API
%%%===================================================================

start(Host, _Opts) ->
    ?INFO_MSG("~p starting~n", [?MODULE]),
    inets:start(),

    mnesia:create_table(?DBT,
                        [{disc_copies, [node()]},
                         {record_name, background_counter},
                         {attributes, record_info(fields, background_counter)}]),
    mnesia:add_table_copy(?DBT, node(), disc_copies),
    mnesia:change_table_copy_type(?DBT, node(), disc_copies),

    ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, on_set_presence, 20),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, on_unset_presence, 20),
    %% mod_offline registers its hook at priority 50 and returns the stop atom
    %% this means no hooks run after it. make sure priority is below 50.
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, on_offline_message, 40),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, on_packet_send, 20),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, on_remove_user, 50),
    ok.

stop (Host) ->
    ?INFO_MSG("~p stopping", [?MODULE]),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, on_remove_user, 50),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, on_packet_send, 20),
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, on_offline_message, 40),
    ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, on_unset_presence, 20),
    ejabberd_hooks:delete(set_presence_hook, Host, ?MODULE, on_set_presence, 20),
    ok.

on_offline_message(From, To, Packet) ->
    case xml:get_tag_attr_s("type", Packet) of
        "chat" ->
            spawn(?MODULE, offline_message, [From, To, Packet]);
        _      -> ok
    end.

offline_message(From, To, Packet) ->
    %% XXX - consider using a different counter here. There be dragons in this
    %% method of counting.
    BC = #background_counter{status = Status, count = V} = read_db(jid_to_id(To)),
    NewCount = case Status of
            background ->
                ?INFO_MSG("User offline via offline message (was in background)", []),
                %% We don't modify the count since packet_send/2 will already have done that
                V;
            _ ->
                V + 1
    end,
    ?INFO_MSG("Offline message count ~p", [NewCount]),
    NewBC = BC#background_counter{status = offline, count = NewCount},
    write_db(NewBC),
    push_notification(NewBC, From, To, Packet).

on_set_presence(User, Server, Resource, Packet) ->
    Id = to_id(User, Server, Resource),
    spawn(?MODULE, set_presence, [Id, Packet]).

set_presence(Id, Packet) -> 
    case xml:get_subtag(Packet, "background") of
         false ->
            ?INFO_MSG("User online", []),
            remove(Id);
         _ -> 
            ?INFO_MSG("User going to background", []),
            mark(Id)       
    end.
    
on_unset_presence(User, Server, Resource, _Status) ->
    Id = to_id(User, Server, Resource),
    spawn(?MODULE, unset_presence, [Id]).

unset_presence(Id) ->
    ?INFO_MSG("User offline from presence", []),
    BC = #background_counter{} = read_db(Id), 
    write_db(BC#background_counter{status = offline}).

on_packet_send(_From, To, Packet) ->
    IsMsg = is_message_packet(Packet),
    Id = jid_to_id(To),
    spawn(?MODULE, packet_send, [IsMsg, Id]).

packet_send(IsMsg, Id) when IsMsg ->
    BC = #background_counter{status = Status, count = V} = read_db(Id),
    case Status of
        background -> 
            write_db(BC#background_counter{count = V + 1});
        _ -> 
            skip
    end;
packet_send(_,_) -> skip.
    
on_remove_user(User, Server) ->
    Id = to_id(User, Server),
    spawn(?MODULE, remove_user, [Id]).

remove_user(Id) ->
    remove(Id).

write_db(#background_counter{} = BC) ->
    {atomic, ok} = mnesia:transaction(
                     fun() ->
                             mnesia:write(?DBT, BC, write)
                     end).

%%%===================================================================
%%% internal functions: presence and counting
%%%===================================================================

to_id(User, Server) ->
    to_id(User, Server, none).
to_id(User, Server, _Resource) ->
    {User, Server}.
jid_to_id(#jid{luser = User, lserver = Server, lresource = Resource}) ->
    to_id(User, Server, Resource).

mark(Id) ->
    F = fun() ->
                BC = fun_read_db(Id),
                mnesia:write(?DBT,  BC#background_counter{status = background}, write)
        end,
    {atomic, _} = mnesia:transaction(F).

remove(Id) ->
    F = fun() -> mnesia:delete(?DBT, Id, write) end,
    {atomic, ok} =  mnesia:transaction(F).

read_db(Id) ->
    {atomic, Res} = mnesia:transaction(fun() -> fun_read_db(Id) end),
    Res.

fun_read_db(Id) ->
    case mnesia:read(?DBT, Id, write) of
        [] -> #background_counter{usr = Id};
        [BC] -> BC
    end.

is_message_packet(_Packet = {xmlelement, "message", Attrs, _Els}) ->
    case xml:get_attr_s("type", Attrs) of
	"groupchat" -> %% mod_muc_log already does it
	    false;
	"error" -> %% we don't log errors
	    false;
	_ ->
	    true
    end;
is_message_packet(_Packet) ->
    false.

%%%===================================================================
%%% internal functions: push notifications
%%%===================================================================
push_notification(BC, From, To, Packet) ->
    %% XXX - Clean up this function, it is too big!
    Body = xml:get_path_s(Packet, [{elem, "body"}, cdata]),
    Thread = xml:get_path_s(Packet, [{elem, "thread"}, cdata]),
    FromS = jlib:jid_to_string(From),
    ToS = jlib:jid_to_string(To),
    UserAgent = "Ejabberd",
    Url = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, service, none),
    Retry = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, retry, true),
    Authorization = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, secret, none),
    PostBody = mochiweb_util:urlencode([{"from", FromS}, {"to", ToS}, {"thread_id", Thread},
                                        {"text", Body}, {"badge", BC#background_counter.count}]),
    Retries = ?MAX_RETRIES,
    R = fun() ->
                Req = {Url,
                       [{"User-Agent", UserAgent}, {"Authorization", Authorization}],
                       "application/x-www-form-urlencoded", PostBody},
                case httpc:request(post, Req, [], []) of
                    {ok, {{_, Code, _}, _, _}} when Code >= 200, Code < 300 ->
                        ok;
                    {ok, {{_, _, _}, _, _} = Reason} ->
                        erlang:error({not_200_code, Reason});
                    {error, Reason} ->
                        erlang:error(Reason);
                    Reason ->
                        erlang:error({unrecognized_answer, Reason})
                end
        end,
    OnFailure = fun(Err) ->
                        ?ERROR_MSG("Failed to push to ~p (~p retries);~nWith last error: ~p", [Url, Retries, Err])
                end,
    case Retry of
        true ->
            F1 = fun() -> retry_at_most(R, Retries, OnFailure) end;
        false ->
            F1 = R
    end,
    spawn(F1).

retry_at_most(Fun, N, OnFailure) ->
     retry_at_most0(Fun, N, 0, OnFailure, none).

retry_at_most0(_, N, N, OnFailure, Err) ->
    OnFailure(Err);
retry_at_most0(Fun, Max, Cur, OnFailure, _) ->
    timer:sleep(erlang:trunc(math:pow(4, Cur)) * 1000),
    try Fun() of
        _ ->
            ok
    catch
        Code:Reason ->
            retry_at_most0(Fun, Max, Cur+1, OnFailure, {Code, Reason})
    end.
