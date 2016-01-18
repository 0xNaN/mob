-include_lib("eunit/include/eunit.hrl").

should_run_a_service_test() ->
    meck:new(service, [non_strict]),
    FakeServicePid = self(),

    meck:expect(service, spawn, fun(_Service) -> {ok, FakeServicePid} end),
    meck:expect(service, start, fun(_ServiceName) -> ok end),

    State = #state{spawned = []},
    Service = #service{name = "myservice", command = "my command", requires = []},
    NewState = handle_run(Service, State),

    ?assertEqual(1, meck:num_calls(service, spawn, [Service])),
    ?assertEqual(1, meck:num_calls(service, start, [Service#service.name])),
    ?assertEqual(#state{spawned = [Service#service.name]}, NewState),

    ?assert(meck:validate(service)),
    meck:unload(service).

should_not_try_to_spawn_an_already_spawned_service_test() ->
    meck:new(service, [non_strict]),

    meck:expect(service, start, fun(_ServiceName) -> ok end),

    State = #state{spawned = ["myservice"]},
    Service = #service{name = "myservice", command = "my command", requires = []},
    NewState = handle_run(Service, State),

    ?assertEqual(1, meck:num_calls(service, start, [Service#service.name])),
    ?assertEqual(State, NewState),
    ?assert(meck:validate(service)),
    meck:unload(service).

should_spawn_but_not_start_a_service_if_its_dependencies_arent_started_test() ->
    meck:new(service, [non_strict]),
    meck:new(mob, [non_strict]),
    FakeServicePid = self(),
    DependencyName = first_dependency,
    State = #state{spawned = [DependencyName]},

    meck:expect(service, spawn, fun(_Service) -> {ok, FakeServicePid} end),
    meck:expect(service, start, fun(_ServiceName) -> ok end),
    meck:expect(service, is_started, fun(_ServiceName) -> false end),
    meck:expect(mob, is_remotely_started, fun(_ServiceName) -> false end),

    Service = #service{name = myservice, command = "my command", requires = [DependencyName]},
    ExpectedState = #state{spawned = [Service#service.name, DependencyName]},
    NewState = handle_run(Service, State),

    ?assertEqual(1, meck:num_calls(service, spawn, [Service])),
    ?assertEqual(1, meck:num_calls(service, is_started, [DependencyName])),
    ?assertEqual(0, meck:num_calls(service, start, [Service#service.name])),
    ?assertEqual(ExpectedState, NewState),
    ?assert(meck:validate(service)),
    ?assert(meck:validate(mob)),
    meck:unload(service),
    meck:unload(mob).

should_search_on_the_network_if_a_dependencies_isnt_found_locally_test() ->
    meck:new(service, [non_strict]),
    meck:new(mob, [non_strict]),

    FakeServicePid = self(),
    DependencyName = first_dependency,
    State = #state{spawned = []},

    meck:expect(service, spawn, fun(_Service) -> {ok, FakeServicePid} end),
    meck:expect(service, start, fun(_ServiceName) -> ok end),
    meck:expect(mob, is_remotely_started, fun(_ServiceName) -> true end),

    Service = #service{name = myservice, command = "my command", requires = [DependencyName]},
    ExpectedState = #state{spawned = [Service#service.name]},
    NewState = handle_run(Service, State),

    ?assertEqual(1, meck:num_calls(service, spawn, [Service])),
    ?assertEqual(1, meck:num_calls(mob, is_remotely_started, [DependencyName])),
    ?assertEqual(1, meck:num_calls(service, start, [Service#service.name])),
    ?assertEqual(ExpectedState, NewState),
    ?assert(meck:validate(service)),
    ?assert(meck:validate(mob)),
    meck:unload(service),
    meck:unload(mob).

