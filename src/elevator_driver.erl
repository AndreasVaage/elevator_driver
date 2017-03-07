-module(elevator_driver).
-author('andrevaa@stud.ntnu.no').

-behavior(gen_server).

%% API functions
-export([
         start_elevator/2,
         set_motor_dir/1,
         set_button_light/3,
         set_door_light/1,
         set_floor_indicator/1,
         set_stop_light/1
        ]).

-spec start_elevator(Module :: module(), simulator|elevator) -> 
    {ok, Pid::pid()} | ignore | {error, {already_started, Pid::pid()} | term()}.

-spec set_motor_dir(up|down|stop) -> ok.
-spec set_button_light(up|down|internal, Floor :: integer(), on|off) -> ok.
-spec set_door_light(on|off) -> ok.
-spec set_floor_indicator(Floor::integer()) -> ok.
-spec set_stop_light(on|off) -> ok.

-callback event_button_pressed({up|down|internal, Floor::integer()}) -> ok.
-callback event_reached_new_floor(Floor::integer()) -> ok.

%% gen_server callbacks
-export([init/1, 
         handle_call/3,
         handle_cast/2, 
         handle_info/2,
         code_change/3,
         terminate/2]).
%% Server state
-record(state, {last_floor, button_list, elevator_type, port, callback_module,
                top_floor = 3,
                number_of_elevators = 1,
                poll_period = 50,
                external_program = "elevator_driver",
                external_timeout = 3000,
                simulator_ip = {127,0,0,1},
                simulator_port = 15657,
                simulator_socket
                }).
%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_elevator(Module, ElevatorType) ->
    gen_server:start_link(
        {local, ?MODULE}, elevator_driver, [Module, ElevatorType], []).

set_motor_dir(Direction) ->
    gen_server:call(?MODULE, {elev_set_motor_dir, Direction}).
set_door_light(State) ->
    gen_server:call(?MODULE, {elev_set_door_open_lamp, State}).
set_stop_light(State) ->
    gen_server:call(?MODULE, {elev_set_stop_lamp, State}).
set_floor_indicator(Floor) ->
    gen_server:call(?MODULE, {elev_set_floor_indicator, Floor}).
set_button_light(ButtonType, Floor, State) ->
    gen_server:call(?MODULE, {elev_l, ButtonType, Floor, State}).
%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([Module, ElevatorType]) ->
    process_flag(trap_exit, true),
    Rec = #state{},
    TopFloor = Rec#state.top_floor,
    io:format("Test output = ~p~n",[TopFloor]),
    State = #state{
    button_list = combine_lists([up, down, internal], lists:seq(0,TopFloor), 0),
    elevator_type = ElevatorType,
    callback_module = Module},
    init_continue(State).

init_continue(#state{elevator_type = elevator} = State) ->
    PrivDir = code:priv_dir(elevator_driver),
    ExtProg = State#state.external_program,
    ExtProgWithPath = filename:join([PrivDir, ExtProg]),
    Port = open_port({spawn_executable, ExtProgWithPath}, [{packet, 2}]),
    {reply, 0} = 
    call_elevator({elev_init, elevator}, Port, State#state.external_timeout),
    init_finnish(State#state{port = Port});

init_continue(#state{elevator_type = simulator} = State) ->
    #state{simulator_ip = Ip, simulator_port = Port} = State,
    {ok, Socket} = gen_tcp:connect(Ip, Port, [binary, {active, false}]),
    gen_tcp:send(Socket, encode({elev_init, simulator})),
    init_finnish(State#state{simulator_socket = Socket}).

init_finnish(State) ->
    io:format("Elevator driver initialised.~n"),
    erlang:send_after(State#state.poll_period, self(), time_to_poll),
    {ok, State}.

handle_call(Msg, _From, #state{port=Port, elevator_type = elevator} = State) ->
    {Command, Reply} = call_elevator(Msg, Port, State#state.external_timeout),
    {Command, Reply, State};

handle_call(Msg, _From, #state{elevator_type = simulator} = State) ->
    gen_tcp:send(State#state.simulator_socket, encode(Msg)),
    {reply,ok, State}.

handle_info(time_to_poll, State) ->
    NewState = poll_buttons(State),
    NewNewState = poll_floor(NewState),
    erlang:send_after(State#state.poll_period, self(), time_to_poll),
    {noreply, NewNewState};

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    io:format("Port closed for reason: ~p~n",[Reason]),
    {stop, {port_terminated, Reason}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate({port_terminated, Reason}, _State) ->
    io:format("Terminating ~p, because port terminated for reason: ~p ~n",
        [?MODULE, Reason]),
    io:format("done~n");
terminate(Reason, #state{port = Port} = _State) ->
    io:format("Terminating ~p, because: ~p!~n",[?MODULE, Reason]),
    io:format("    Closing port~n"),
    port_close(Port),
    io:format("done~n").

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

combine_lists(L1, L2, Value) ->
    [{{X, Y}, Value} || X <- L1, Y <- L2].

poll_buttons(#state{top_floor = TopFloor} = State) ->
    N = 3*(TopFloor + 1),
    poll_buttons(State, N).

poll_buttons(State, 0) ->
    State;

poll_buttons(State, N) ->
    #state{button_list = List, port = Port, external_timeout = Timeout} = State,
    {Button, LastValue} = lists:nth(N, List),
    Return = case State#state.elevator_type of
        simulator ->
            call_simulator({elev_get_order, Button}, State#state.simulator_socket, Timeout);
        elevator ->
            call_elevator({elev_get_order, Button}, Port, Timeout)
        end,
    case Return of
        {reply, LastValue} -> %No state change
            poll_buttons(State, N-1);
        {reply, 1} -> %Button pressed
            Mod = State#state.callback_module,
            Mod:event_button_pressed(Button),
            NewList = lists:keyreplace(Button, 1, List, {Button, 1}),
            poll_buttons(State#state{button_list = NewList}, N-1);
        {reply, 0} -> %Button released
            NewList = lists:keyreplace(Button, 1, List, {Button, 0}),
             poll_buttons(State#state{button_list = NewList}, N-1);
        {stop, port_timeout} -> 
            exit(port_timeout);
        Undefined -> 
            io:format("Elevator driver undefined input: ~p~n",[Undefined])
    end.

poll_floor(State) ->
    #state{
    port = Port, last_floor = LastFloor, external_timeout = Timeout
    } = State,
    Return = case State#state.elevator_type of
        simulator ->
            call_simulator({elev_get_floor_sensor_signal}, State#state.simulator_socket, Timeout);
        elevator ->
            call_elevator({elev_get_floor_sensor_signal}, Port, Timeout)
    end,
    case Return of
        {reply, LastFloor} -> %No change
            State;
        {reply, 255} -> % We don´t care of leaving and arriving. yet.
            State;
        {reply, NewFloor} ->
            Mod = State#state.callback_module,
            Mod:event_reached_new_floor(NewFloor),
            State#state{last_floor = NewFloor};
        {stop, port_timeout} ->
            exit(port_timeout)
    end.

call_elevator(Msg, Port, Timeout) ->
    port_command(Port, encode(Msg)),
    receive
        {Port, {data, [Data]}} ->
            {reply, Data}
    %% Prevent the gen_server from hanging indefinitely in case the
    %% spawned process is taking too long processing the request.
    after Timeout -> 
            {stop, port_timeout}
    end.

call_simulator(Msg, Socket, Timeout) ->
    gen_tcp:send(Socket, encode(Msg)),
    case gen_tcp:recv(Socket, 4, Timeout) of
        {ok, Packet} ->
            {reply, decode(Packet)};
        {error, closed} ->
            {stop, simulator_socket_closed};
        {error, timeout} ->
            {stop, simulator_timeout};
        {error, Reason} ->
            {stop, Reason};
        SomethingElse ->
            io:format("call_simulator, gen_tcp:recv returned ~p~n",
                [SomethingElse])
    end.

encode({elev_init, elevator}) -> [0, 0];
encode({elev_init, simulator}) -> [0, 1];
encode({elev_set_motor_dir, stop}) -> [1, 0];
encode({elev_set_motor_dir, up}) -> [1, 1];
encode({elev_set_motor_dir, down}) -> [1, 255];
encode({elev_l, up, Floor, on}) -> [2, 0, Floor, 1];
encode({elev_l, up, Floor, off}) -> [2, 0, Floor, 0];
encode({elev_l, down, Floor, on}) -> [2, 1, Floor, 1];
encode({elev_l, down, Floor, off}) -> [2, 1, Floor, 0];
encode({elev_l, internal, Floor, on}) -> [2, 2, Floor, 1];
encode({elev_l, internal, Floor, off}) -> [2, 2, Floor, 0];
encode({elev_set_floor_indicator, Floor}) -> [3, Floor];
encode({elev_set_door_open_lamp, off}) -> [4, 0];
encode({elev_set_door_open_lamp, on}) -> [4, 1];
encode({elev_set_stop_lamp, off}) -> [5, 0];
encode({elev_set_stop_lamp, on}) -> [5, 1];
encode({elev_get_order, {up, Floor}}) -> [6, 0, Floor];
encode({elev_get_order, {down, Floor}}) -> [6, 1, Floor];
encode({elev_get_order, {internal, Floor}}) -> [6, 2, Floor];
encode({elev_get_floor_sensor_signal}) -> [7];
encode({elev_get_stop_signal}) -> [8];
encode({elev_get_obstruction_signal}) -> [9].

decode(<<6,1,0,0>>) -> 1;
decode(<<6,0,0,0>>) -> 0;
decode(<<7,1,Floor,0>>) -> Floor;
decode(<<7,0,_Floor,0>>) -> 255;
decode(<<8,0,0,0>>) -> 0;
decode(<<8,1,0,0>>) -> 1;
decode(<<9,0,0,0>>) -> 0;
decode(<<9,1,0,0>>) -> 1.