-module(network).
-compile([export_all]).

-define (TIMEOUT_REQUEST, 500).

-record(state, { knowledge, %% MUST be always ordered ASC, max size K
				 active,    %% MUST be always ordered ASC, max size K
				 contacted
				}).

find_peers(Peer, Key, K, Alpha) ->
    FindPeersCall = fun(Contact) -> peer:find_closest_peers(Contact, Key, Peer) end,
    find(Peer, Key, K, Alpha, FindPeersCall).

find_value(Peer, Key, K, Alpha) ->
    FindValueCall = fun(Contact) -> peer:find_value_of(Contact, Key, Peer) end,
    find(Peer, Key, K, Alpha, FindValueCall).

find(Peer, Key, K, Alpha, FindCall) ->
    spawn(network, do_find, [self(), Peer, Key, FindCall, K, Alpha]),
    receive
        {ok, Reply} -> Reply
    end.

do_find(Client, Peer, Key, FindCall, K, Alpha) ->
    FindState = #state {
                   knowledge  = peer:closest_to(Peer, Key),
                   active     = [Peer],
                   contacted  = [Peer]
                },

    Reply = find_iterator(FindState, Key, FindCall, K, Alpha),
    Client ! {ok, Reply}.

find_iterator(FindState, Key, FindCall, K, AsynQueries) ->
	query_peers(FindState, AsynQueries, FindCall, self()),
    receive
        {ok, [], [], []} ->
            FindState#state.active;

        {ok, [], [], Expired} ->
            NewFindState = FindState#state {
                              contacted = append_unique(FindState#state.contacted, Expired)
                           },

            AllKnowledge = length(NewFindState#state.knowledge),
            find_iterator(NewFindState, Key, FindCall, K, AllKnowledge);

        {ok, Discovered, Active, Expired} ->

            NewFindState = FindState#state {
                              knowledge = k_closest_to(K, Key, append_unique(Discovered, FindState#state.knowledge)),
                              active    = k_closest_to(K, Key, append_unique(Active, FindState#state.active)),
                              contacted = append_unique(FindState#state.contacted, Active ++ Expired)
							},

            [CandidateClosest   | _ ] = kbucket:sort_on(Key, Active),
            [ClosestActiveSoFar | _ ] = FindState#state.active,

            case kbucket:is_closest(CandidateClosest, ClosestActiveSoFar, Key) of
                true ->
                    find_iterator(NewFindState, Key, FindCall, K, AsynQueries);
                false ->
                    % if a round doesn't returns any peer closer (CandidateClosest)
                    % than the closest already seen (ClosestActiveSoFar) sends the find_*
                    % to all of the K closest peers not already queried
                    AllKnowledge = length(NewFindState#state.knowledge),
                    find_iterator(NewFindState, Key, FindCall, K, AllKnowledge)
            end;

         {ok, found, Value} ->
            {found, Value}
    end.

query_peers(FindState, AsynQueries, FindCall, FindIterator)->
    SelectedPeers = pick_n_not_queried(FindState, AsynQueries),
    ReplyExpected = length(SelectedPeers),
    FindCollector = spawn(network, find_collector, [ReplyExpected, FindIterator]),
    FindWorker    = fun(Peer) -> spawn(network, find_worker, [Peer, FindCall, FindCollector]) end,
    lists:foreach(FindWorker, SelectedPeers).

find_collector(ReplyExpected, FindIterator) ->
	find_collector(ReplyExpected, FindIterator, [], [], []).

find_collector(0, FindIterator, Discovered, Active, Expired) ->
    FindIterator ! {ok, Discovered, Active, Expired};
find_collector(ReplyExpected, FindIterator, Discovered, Active, Expired) ->
    receive
        {response_from, _Peer, {found, Value}} ->
            FindIterator ! {ok, found, Value};
        {response_from, Peer, Result} ->
            NewResult = lists:merge(Result, Discovered),
            find_collector(ReplyExpected - 1, FindIterator, NewResult, [Peer | Active], Expired);
        {timeout_from, Peer} ->
            find_collector(ReplyExpected - 1, FindIterator, Discovered, Active, [Peer | Expired])
    end.

find_worker(Peer, FindCall, FindCollector) ->
    try FindCall(Peer) of
      Result -> FindCollector ! {response_from, Peer, Result}
    catch
      exit:_ -> FindCollector ! {timeout_from, Peer}
    end.

append_unique(FirstList, SecondList) ->
    List = lists:append(FirstList, SecondList),
    sets:to_list(sets:from_list(List)).

k_closest_to(K, Key, Contacts) ->
    SortedContacts = kbucket:sort_on(Key, Contacts),
    lists:sublist(SortedContacts, K).

pick_n_not_queried(FindState, N) ->
    lists:sublist(FindState#state.knowledge -- FindState#state.contacted, N).
