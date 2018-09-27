-module(genserver).
-export([start/3, stop/1, request/2, request/3, update/2]).

% Spawn a server process and register it with a given atom
%
%  1. Server: the atom the process should be registed to
%  2. State: the initial state of the server loop
%  3. Handler: the function implementing the functionality
%
% The server process is the loop function and can be called
% upon using its registered name. The handler function takes 
% two arguments:
%
%  1. State: the current state of the server
%  2. Request: the request message
%
% It returns a tupel containing the result of the request and
% the new server state.

start(Server, State, Handler) ->
  Pid = spawn(fun() -> loop(State, Handler) end),
  catch(unregister(Server)),
  register(Server, Pid),
  Pid.

stop(Server) ->
  Server ! stop,
  catch(unregister(Server)),
  ok.

% The server functionality is enclosed in this loop function
% and is interacted with using the interface methods that are
% exported from this module.

loop(State, Handler) ->
  receive

    {request, From, Ref, Request} ->
      case catch(Handler(State, Request)) of
        {'EXIT', Reason} ->
          From ! {exit, Ref, Reason},
          loop(State, Handler);
        {reply, Result, NewState} ->
          From ! {result, Ref, Result},
          loop(NewState, Handler)
        end;

    {update, From, Ref, NewH} ->
      From ! {ok, Ref},
      loop(State, NewH);

    stop -> true

  end.

% The generic server's 'request' function takes care of sending 
% requests to the server, and of sending back the result to the
% process making the request call.

request(Server, Request) ->
  request(Server, Request, 3000).

% Send a request to a server (specified by its pid) and wait for 
% a response with a specified timeout. If the Server is an atom 
% which is not registered, a badarg exception will be thrown.

request(Server, Request, Timeout) ->
  Ref = make_ref(),
  Server ! {request, self(), Ref, Request},
  receive
    {result, Ref, Result} ->
      Result;
    {exit, Ref, Reason} ->
      exit(Reason)
  after Timeout ->
    exit("Timeout")
  end.

% Flexibility is increased with the ability to update the server's
% functionality while it is running. This requires a new kind of 
% request that changes the handling function. The 'update' function
% below takes care of informing the server (specified by its pid) 
% to update the function to the one it provides.

update(Server, Handler) ->
  Ref = make_ref(),
  Server ! {update, self(), Ref, Handler},
  receive
    {ok, Ref} ->
      ok
  end.
