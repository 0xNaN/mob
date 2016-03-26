-module(network).
-compile([export_all]).

-define (TIMEOUT_REQUEST, 500).

find_peers(ExecutorContact, Key, Alpha) ->
    FindPeersCall = fun(Contact) -> peer:find_closest_peers(Contact, Key, ExecutorContact) end,
    BaseKnowlegeContact = peer:closest_to(ExecutorContact, Key),
    spawn(network, find_monitor, [self(), ExecutorContact, Key, BaseKnowlegeContact, FindPeersCall, Alpha]),
    receive
        {ok, Peers} -> Peers
    end.

find_value(ExecutorContact,  Key, Alpha) ->
    FindValueCall = fun(Contact) -> peer:find_value_of(Contact, Key, ExecutorContact) end,
    BaseKnowlegeContact = peer:closest_to(ExecutorContact, Key),
    spawn(network, find_monitor, [self(), ExecutorContact, Key, BaseKnowlegeContact, FindValueCall, Alpha]),
    receive
        {ok, Peers} -> Peers
    end.

find_monitor(Parent, ExecutorContact, Key, BaseKnowlegeContact, FindCall, Alpha) ->
    Ret = find_worker(ExecutorContact, BaseKnowlegeContact, Key, [ExecutorContact], [], FindCall, Alpha),
    Parent ! {ok, Ret}.


find_worker(_ExecutorContact, [], _Key, BestActive, _Contacted, _FindCall, _Alpha) ->
    BestActive;
find_worker(ExecutorContact, KnowlegeContacts, Key, BestActive, Contacted, FindCall, Alpha) ->
    Selected = lists:sublist(KnowlegeContacts -- Contacted, Alpha),
    %% spawn_link
    AlphaFindCollector = spawn(network, alpha_find_collector,
                               [min(Alpha,length(Selected)), self(), [], [], []]),
    lists:foreach(fun(Contact) -> spawn(network, discover_from,
                                        [Contact, FindCall, AlphaFindCollector]) end, Selected),
    receive
        {ok, [], [], []} ->
            BestActive;

        {ok, [], [], Expired} ->
            NewContacted = append_unique(Contacted, Expired),
            NewKnowlege = KnowlegeContacts -- Contacted,
            UnaskedNumber = length(NewKnowlege),
            find_worker(ExecutorContact, NewKnowlege, Key, BestActive, NewContacted, FindCall, UnaskedNumber);

        {ok, Discovered, Active, Expired} ->
            NewContacted = append_unique(Contacted,  Active ++ Expired),
            CompleteKnowlege = append_unique(Discovered, (KnowlegeContacts -- NewContacted)),
            SortedKnowlege = peer:k_closest_to(ExecutorContact, Key, CompleteKnowlege),


            [CandidateClosest | _ ] = peer:k_closest_to(ExecutorContact, Key, Active),
            [ClosestSoFar | _] = peer:k_closest_to(ExecutorContact, Key, BestActive),

            NewBestActive = peer:k_closest_to(ExecutorContact, Key, append_unique(Active, BestActive)),
            UnaskedNumber = length(KnowlegeContacts),
            case kbucket:is_closest(CandidateClosest, ClosestSoFar, Key) of
                true ->
                    find_worker(ExecutorContact, SortedKnowlege, Key, NewBestActive, NewContacted, FindCall, Alpha);
                false ->
                    find_worker(ExecutorContact, SortedKnowlege, Key, NewBestActive, NewContacted, FindCall, UnaskedNumber)
            end;

         {ok, found, Value} ->
            {found, Value}
    end.

alpha_find_collector(0, Worker, Discovered, Active, Expired) ->
    Worker ! {ok, lists:flatten(Discovered), lists:flatten(Active), lists:flatten(Expired)};
alpha_find_collector(Alpha, Worker, Discovered, Active, Expired) ->
    receive
        {response_from, _Contact, {found, Value}} ->
            Worker ! {ok, found, Value};
        {response_from, Contact, Result} ->
            alpha_find_collector(Alpha - 1, Worker, Result ++ Discovered, [Contact | Active], Expired);

        {timeout_from, Contact} ->
            alpha_find_collector(Alpha - 1, Worker, Discovered, Active, [Contact | Expired])
    end.

discover_from(Contact, FindCall, FindWorker) ->
    try FindCall(Contact) of
      Result -> FindWorker ! {response_from, Contact, Result}
    catch
      exit:_ -> FindWorker ! {timeout_from, Contact}
    end.

append_unique(FirstList, SecondList) ->
    List = lists:append(FirstList, SecondList),
    sets:to_list(sets:from_list(List)).
