-module(check_link_handler).
-behaviour(gen_event).

-export([init/1]).
-export([code_change/3]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([terminate/2]).

-include("peer.hrl").

-define(TIMEOUT, 1500).

-record(state, {peer, from_contact, owner}).


init([Peer, FromContact, From]) ->
  erlang:send_after(?TIMEOUT, self(), timeout),
	{ok, #state{peer = Peer, from_contact = FromContact, owner = From}}.

handle_event({pong, _From, FromContact, []}, #state{from_contact = From, owner = Owner})
  when FromContact =:= From ->
    gen_server:reply(Owner, ok),
    remove_handler;
handle_event(_Event, State) ->
   {ok, State}.

handle_info(timeout, #state{owner = Owner}) ->
   gen_server:reply(Owner, ko),
   remove_handler;
handle_info(_Info, State) ->
   {ok, State}.


handle_call(_Request, Peer) ->
   {ok, ok, Peer}.


terminate(_Reason, _Peer) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
