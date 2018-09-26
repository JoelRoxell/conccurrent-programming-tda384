-module(server).

-export([start/1, stop/1]).

-record(user, {pid, nick}).

-record(state, {users = [], channels = #{}}).

-record(client_st,
	{gui, % atom of the GUI process
	 nick, % nick/username of the client
	 server}). % atom of the chat server
% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    io:fwrite("Starting server2..."),
    InitialState = #state{},
    ServerPid = spawn(fun () ->
			      loop(ServerAtom, InitialState)
		      end),
    register(ServerAtom, ServerPid),
    ServerPid,
    ok.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) -> exit(whereis(ServerAtom), kill), ok.

loop(ServerAtom, State) ->
    receive
      % Join channel
      {Sender, GuidPid, Channel, Nick} ->
        Channels =  State#state.channels,

        case maps:is_key(Channel, Channels) of 
          false -> 
            NewChannels = maps:put(Channel, [#user{pid=GuidPid, nick = Nick}], Channels),

            Sender ! {true},
            loop(ServerAtom, #state{ users = State#state.users, channels = NewChannels });
          true -> 
            OldChannel = maps:get(Channel, Channels),
            NewUser = #user{pid=GuidPid, nick = Nick},
            IsInChannel = lists:member(NewUser, OldChannel),

            if 
              IsInChannel -> 
                Sender ! {false};
              true -> 
                NewChannels = maps:put(Channel,  OldChannel ++ [NewUser], Channels),

                Sender ! {true},
                loop(ServerAtom, #state{ users = State#state.users, channels = NewChannels })
            end
        end;

      % Receive message
      {SenderGuidPid, Channel, Nick, Message, message_send} ->
        % Notify all group members.
        % genserver:request(GuidPid, {message_receive, Channel, Nick, Message}),
        lists:map(fun(User) -> 
          if 
            User#user.pid == SenderGuidPid -> none;
            true -> client:handle(#client_st{gui = User#user.pid}, {message_receive, Channel, Nick, Message})
          end
        end, maps:get(Channel, State#state.channels)),  

        % io:fwrite("~p~n", [Result]),
        loop(ServerAtom, State)
        
    end.
