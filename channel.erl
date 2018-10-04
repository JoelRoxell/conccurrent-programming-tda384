-module(channel).
-export([start/1, stop/1, handle/2]).

-record(channel_state, {name, members}).
-record(client_state, {pid, gui, nick, server, channels}).

% Start a new channel process with the given name:
start(Channel) ->
    S = #channel_state{name = Channel, members = dict:new()},
    genserver:start(Channel, S, fun channel:handle/2).

% Stop the server process registered to the given name
% together with any other associated processes:
stop(Channel) ->
    genserver:stop(Channel).

% Make sure that a message sent from one channel member
% is receied by all the other clients part of that same 
% conversation:
send_to_members(Channel, Receivers, Nick, Msg) ->
    %io:format("4. inside send_to_members function~n"),
    %io:format("5. receivers: ~w~n", [Receivers]),
    dict:map(fun(_, Pid) -> 
        genserver:request(Pid, {message_receive, Channel, Nick, Msg}) end,
    Receivers).

% -----------------------------------------------------------------------------
% THE HANDLE FUNCTION FOR THE CHANNEL SERVER
% -----------------------------------------------------------------------------

handle(S, {join, Client, Pid}) ->
    %io:format("4. add user to channel~n"),
    Members = dict:store(Client#client_state.nick, Pid, S#channel_state.members),
    {reply, ok, S#channel_state{members = Members}};

handle(S, {leave, Nick}) ->
    Members = dict:erase(Nick, S#channel_state.members),
    {reply, ok, S#channel_state{members = Members}};

handle(S, {change_nick, Client, NewNick}) ->
    Discard = dict:erase(Client#client_state.nick, S#channel_state.members),
    Members = dict:store(NewNick, Client#client_state{nick = NewNick}, Discard),
    {reply, ok, S#channel_state{members = Members}};

handle(S, {message_send, Nick, Channel, Msg}) ->
    %io:format("2. message received by channel~n"),
    Receivers = dict:erase(Nick, S#channel_state.members),
    %io:format("3. message distributed to members~n"),
    send_to_members(Channel, Receivers, Nick, Msg),
    {reply, ok, S}.