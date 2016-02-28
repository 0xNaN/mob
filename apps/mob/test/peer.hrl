-include_lib("eunit/include/eunit.hrl").
-include_lib("test_macro.hrl").

-define(FAKE_PEER, {self(), 2#0101}).

start() ->
    meck:new(kbucket, [non_strict]),
    KbucketPid = self(),
    Id = 1,
    Alpha = 3,
    Peer = peer:start(Id, KbucketPid, Alpha),
    {Peer, KbucketPid}.

teardown({_, _}) ->
    ?assert(meck:validate(kbucket)),
    meck:unload(kbucket).

peer_suite_test_() ->
     [?setup(fun should_store_data/1),
      ?setup(fun should_overwrite_data_with_same_key/1),
      ?setup(fun should_answer_with_pong_to_a_ping/1),
      ?setup(fun should_update_kbucket_if_receive_a_pong/1),
      ?setup(fun should_contact_the_kbucket_for_its_closest_peer_to_a_key/1),
      ?setup(fun should_returns_its_closest_peers_when_no_key_is_found/1),
      ?setup(fun should_not_returns_the_contact_of_the_caller/1)].

should_store_data({Peer, KbucketPid}) ->
    FakePeer = ?FAKE_PEER,
    {Key, Value} = {mykey, "myvalue"},

    meck:expect(kbucket, put, ?two_any_args(?return(ok))),
    peer:store(Peer, {Key, Value}, FakePeer),

    [?_assertEqual({found, "myvalue"}, peer:find_value_of(Peer, Key, FakePeer)),
     ?_assertEqual(2, meck:num_calls(kbucket, put, [KbucketPid, FakePeer]))].

should_overwrite_data_with_same_key({Peer, KbucketPid}) ->
    FakePeer = ?FAKE_PEER,
    {Key, FirstValue} = {mykey, "myvalue"},
    SecondValue = "updated",

    meck:expect(kbucket, put, ?two_any_args(?return(ok))),
    peer:store(Peer, {Key, FirstValue},  FakePeer),
    peer:store(Peer, {Key, SecondValue}, FakePeer),

    [?_assertEqual({found, SecondValue}, peer:find_value_of(Peer, Key, FakePeer)),
     ?_assertEqual(3, meck:num_calls(kbucket, put, [KbucketPid, FakePeer]))].

should_answer_with_pong_to_a_ping({Peer, KbucketPid}) ->
    FakePeer = ?FAKE_PEER,

    meck:expect(kbucket, put, ?two_any_args(?return(ok))),
    peer:ping(Peer, FakePeer),

    ?receiving({_ ,{rpc, pong, FromPeer, []}},
               [?_assertEqual(FromPeer, Peer),
                ?_assert(meck:called(kbucket, put, [KbucketPid, FakePeer]))]).

should_update_kbucket_if_receive_a_pong({Peer, KbucketPid}) ->
    FakePeer = ?FAKE_PEER,

    meck:expect(kbucket, put, ?two_any_args(?return(ok))),
    peer:pong(Peer, FakePeer),

    [?_assert(meck:called(kbucket, put, [KbucketPid, FakePeer]))].

should_contact_the_kbucket_for_its_closest_peer_to_a_key({Peer, KbucketPid}) ->
    FakePeer = ?FAKE_PEER,
    KeyToSearch = 2,

    meck:expect(kbucket, closest_contacts, ?two_any_args(?return([1, 2]))),
    meck:expect(kbucket, put, ?two_any_args(?return(ok))),

    [?_assertEqual([1, 2], peer:find_closest_peers(Peer, KeyToSearch, FakePeer)),
     ?_assert(meck:called(kbucket, put, [KbucketPid, FakePeer])),
     ?_assert(meck:called(kbucket, closest_contacts, [KbucketPid, KeyToSearch]))].

should_returns_its_closest_peers_when_no_key_is_found({Peer, KbucketPid}) ->
    FakePeer = ?FAKE_PEER,
    UnknownKey = 2,

    meck:expect(kbucket, closest_contacts, ?two_any_args(?return([1, 2]))),
    meck:expect(kbucket, put, ?two_any_args(?return(ok))),

    [?_assertEqual([1, 2], peer:find_value_of(Peer, UnknownKey, FakePeer)),
     ?_assertEqual(1, meck:num_calls(kbucket, put, [KbucketPid, FakePeer])),
     ?_assert(meck:called(kbucket, closest_contacts, [KbucketPid, UnknownKey]))].

should_not_returns_the_contact_of_the_caller({Peer, _}) ->
    FakePeer = ?FAKE_PEER,
    KeyToSearch = 1,

    meck:expect(kbucket, closest_contacts, ?two_any_args(?return([2, FakePeer]))),
    meck:expect(kbucket, put, ?two_any_args(?return(ok))),

    [?_assertEqual([2], peer:find_closest_peers(Peer, KeyToSearch, FakePeer))].
