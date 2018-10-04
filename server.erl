-module(server).
-export([start/1, stop/1, handle/2]).

-record(server_state, {clients, channels}).
-record(client_state, {pid, gui, nick, server, channels}).

% Start a new server process with the given name (do not 
% change the signature):
start(Server) ->
    S = #server_state{clients = sets:new(), channels = dict:new()},
    genserver:start(Server, S, fun server:handle/2).

% Stop the server process registered to the given name
% together with any other associated processes:
stop(Server) ->
    genserver:stop(Server).

% -----------------------------------------------------------------------------
% THE HANDLE FUNCTION FOR THE CCHAT SERVER
% -----------------------------------------------------------------------------

handle(S, {join, Client, Channel}) ->
    Clients = case sets:is_element(Client#client_state.nick, S#server_state.clients) of
        true ->
            S#server_state.clients;
        false ->
            sets:add_element(Client#client_state.nick, S#server_state.clients)
    end,
    case dict:find(Channel, S#server_state.channels) of
        {ok, Pid} ->
            genserver:request(Pid, {join, Client}),
            {reply, Pid, S#server_state{clients = Clients}};
        error ->
            NewPid = channel:start(list_to_atom(Channel)),
            genserver:request(NewPid, {join, Client}),
            Channels = dict:store(Channel, NewPid, S#server_state.channels),
            {reply, NewPid, S#server_state{clients = Clients, channels = Channels}}
    end;

handle(S, {change_nick, OldNick, NewNick}) ->
    case sets:is_element(NewNick, S#server_state.clients) of
        true ->
            {reply, {error, nick_taken, "This nick isn't unique"}, S};
        false ->
            case sets:is_element(OldNick, S#server_state.clients) of
                true ->
                    Remove = sets:del_element(OldNick, S#server_state.clients),
                    Change = sets:add_element(NewNick, Remove),
                    {reply, ok, S#server_state{clients = Change}};
                false ->
                    Clients = sets:add_element(NewNick, S#server_state.clients),
                    {reply, ok, S#server_state{clients = Clients}}
            end
    end.