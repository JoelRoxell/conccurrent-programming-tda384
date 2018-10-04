-module(client).
-export([handle/2, initial_state/3]).

% The structure of the client state (add whatever fields you need):
-record(client_state, {pid, gui, nick, server, channels}).

% Returns an initial state record (do not modify the signature):
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_state{pid = list_to_atom(Nick), gui = GUIAtom, nick = Nick, 
                    server = ServerAtom, channels = dict:new()}.

update_nick(Client, NewNick) ->
    dict:map(fun(_, Pid) -> 
        genserver:request(Pid, {change_nick, Client, NewNick}) end, 
    Client#client_state.channels).

% The function below (handle/2) manage each kind of request from
% the GUI. The two parameters are:
%   - the current client state
%   - the specific GUI request
% The function returns a tuple {reply, Request, NewState} where:
%   - Request is the reply returned to the GUI (either the
%     atom 'ok' or the tuple {error, Atom, "Error Message"})
%   - NewState is the updated client state

% Join channel:
handle(S, {join, Channel}) ->
    case dict:find(Channel, S#client_state.channels) of
        {ok,_} ->
            {reply, {error, user_already_joined, "You've already joined"}, S};
        error ->
            case catch(genserver:request(S#client_state.server, {join, S, Channel})) of
                {'EXIT', Reason} ->
                    {reply, {error, server_not_reached, Reason}, S};
                Pid ->
                    Channels = dict:store(Channel, Pid, S#client_state.channels),
                    {reply, ok, S#client_state{channels = Channels}}
            end
    end;

% Leave channel:
handle(S, {leave, Channel}) ->
    case dict:find(Channel, S#client_state.channels) of
        {ok, Pid} ->
            genserver:request(Pid, {leave, S#client_state.nick}),
            Channels = dict:erase(Channel, S#client_state.channels),
            {reply, ok, S#client_state{channels = Channels}};
        error ->
            {reply, {error, user_not_joined, "You haven't joined"}, S}
    end;

% Sending message (from GUI, to channel):
handle(S, {message_send, Channel, Msg}) ->
    case dict:find(Channel, S#client_state.channels) of
        {ok, Pid} ->
            io:format("send message to channel~n"),
            genserver:request(Pid, {message_send, S#client_state.nick, Channel, Msg}),
            {reply, ok, S};
        error ->
            {reply, {error, user_not_joined, "You haven't joined"}, S}
    end;

% -----------------------------------------------------------------------------
% ALREADY IMPLEMENTED CASES
% -----------------------------------------------------------------------------

% Get current nick:
handle(S, whoami) ->
    {reply, S#client_state.nick, S};

% Change nick:
handle(S, {nick, NewNick}) ->
    Res = case catch(genserver:request(S#client_state.server, {change_nick, S#client_state.nick, NewNick})) of
        {'EXIT', Reason} ->
            {error, server_not_reached, Reason};
        Result -> Result
    end,
    update_nick(S, NewNick),
    {reply, Res, S#client_state{nick = NewNick}};

% Incoming message (from channel, to GUI):
handle(S = #client_state{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    io:format("message from server received by client~n"),
    gen_server:call(GUI, {message_receive, Channel, Nick ++ "> " ++ Msg}),
    io:format("message printed onto gui~n"),
    {reply, ok, S};

% Quit client via GUI:
handle(S, quit) ->
    {reply, ok, S};

% Catch-all for any unhandled requests:
handle(S, _Request) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, S}.