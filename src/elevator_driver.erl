-module(elevator_driver).
-author('andrevaa@stud.ntnu.no').

-behavior(gen_server).

%% External exports
-export([start_elevator/2, start_elevator/1]).

%% API functions
-export([
         set_motor_dir/1,        %up/down/stop
         set_button_light/3,     %up/down/internal , int , on/off
         set_door_light/1,       %on/off
         set_floor_indicator/1,  % int
         set_stop_light/1        % on/off
        ]).

-callback event_button_pressed({up|down|internal, Floor :: integer()}) -> ok.
-callback event_reached_new_floor(Floor :: integer()) -> ok.
-spec set_motor_dir(up|down|stop) -> ok.
-spec set_button_light(up|down|internal, Floor :: integer(), on|off) -> ok.
-spec set_door_light(on|off) -> ok.
-spec set_floor_indicator(Floor :: integer()) -> ok.
-spec set_stop_light(on|off) -> ok.

%% gen_server callbacks
-export([init/1, 
         handle_call/3,
         handle_cast/2, 
         handle_info/2,
         code_change/3,
         terminate/2]).
%% Server state
-record(state, {state ,port, top_floor, last_floor, poll_period, button_list,
                elevator_type}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_elevator(Module, ElevatorType) ->
    application:set_env(elevator_driver, environment, ElevatorType),
    start_elevator(Module).
 
start_elevator(Module) ->
    application:set_env(elevator_driver, callback_module, Module),
    {ok, ElevatorType} = application:get_env(elevator_driver, environment),
    gen_server:start_link({local, ?MODULE}, elevator_driver, ElevatorType, []).

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

init(ElevatorType) ->
    process_flag(trap_exit, true),
    TopFloor = get_env(number_of_floors) - 1,
    State = #state{
    button_list = init_list([up, down, internal], lists:seq(0, TopFloor), 0),
    poll_period = get_env(poll_period),
    last_floor = unknown,
    top_floor = TopFloor,
    elevator_type = ElevatorType},
    init_continue(State).

init_continue(#state{elevator_type = elevator} = State) ->
    %Locate the executable
    PrivDir = code:priv_dir(elevator_driver),
    {ok, ExtProg} = application:get_env(elevator_driver, extprog),
    ExtProgWithPath = filename:join([PrivDir, ExtProg]),

    Port = open_port({spawn_executable, ExtProgWithPath}, [{packet,2}]),
    {reply, 0} = call_port({elev_init, elevator}, Port),
    io:format("Elevator driver initialised.~n"),
    erlang:send_after(State#state.poll_period, self(), time_to_poll),
    {ok, State#state{port = Port}};

init_continue(#state{elevator_type = simulator} = State) ->
    %Locate the executable
    PrivDir = code:priv_dir(elevator_driver),
    {ok, ExtProg} = application:get_env(elevator_driver, extprog),
    ExtProgWithPath = filename:join([PrivDir, ExtProg]),

    Port = open_port({spawn_executable, ExtProgWithPath}, [{packet,2}]),
    {reply, 0} = call_port({elev_init, simulator}, Port),
    io:format("Elevator driver initialised.~n"),
    erlang:send_after(State#state.poll_period, self(), time_to_poll),
    {ok, State#state{port = Port}}.

handle_call(Msg, _From, #state{port=Port, elevator_type=elevator} = State) ->
    {Command, Reply} = call_port(Msg, Port),
    {Command, Reply, State};

handle_call(Msg, _From, #state{port=Port, elevator_type=simulator} = State) ->
    {Command, Reply} = call_port(Msg, Port),
    {Command, Reply, State}.

handle_info(time_to_poll, State) ->
    %io:format("I am now polling for the ~pth time!~n",[Count]),
    NewState = State#state{
    button_list = poll_buttons(State#state.button_list, State#state.port),
    last_floor = poll_floor(State#state.last_floor, State#state.port)},
    erlang:send_after(State#state.poll_period, self(), time_to_poll),
    {noreply, NewState};


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
get_env(Environment) -> 
    {ok, Value} = application:get_env(elevator_driver, Environment),
    Value.
    
init_list(L1, L2, Value) ->
    [{{X, Y}, Value} || X <- L1, Y <- L2].

poll_buttons(List, Port) ->
    N = 3*get_env(number_of_floors),
    poll_buttons(List, N, Port).

poll_buttons(List, 0, _Port) ->
    List;

poll_buttons(List, N, Port) ->
    {Button, LastValue} = lists:nth(N, List),
    case call_port({elev_get_order, Button}, Port) of
        {reply, LastValue} -> %No state change
            poll_buttons(List, N-1, Port);
        {reply, 1} -> %Button pressed
            Mod = get_env(callback_module),
            Mod:event_button_pressed(Button),
            NewList = lists:keyreplace(Button, 1, List, {Button, 1}),
            poll_buttons(NewList, N-1, Port);
        {reply, 0} -> %Button released
            NewList = lists:keyreplace(Button, 1, List, {Button, 0}),
            poll_buttons(NewList, N-1, Port);
        {stop, port_timeout} -> 
            exit(port_timeout);
        Undefined -> 
            io:format("Elevator driver undefined input: ~p~n",[Undefined])
    end.

poll_floor(LastFloor, Port) ->
    case call_port({elev_get_floor_sensor_signal}, Port) of
        {reply, LastFloor} -> %No change
            LastFloor;
        {reply, 255} -> % We don´t care of leaving and arriving. yet.
            LastFloor;
        {reply, Floor} ->
            Mod = get_env(callback_module),
            Mod:event_reached_new_floor(Floor),
            Floor;
        {stop, port_timeout} ->
            exit(port_timeout)
    end.

call_port(Msg, Port) ->
    port_command(Port, encode(Msg)),
    receive
        {Port, {data, [Data]}} ->
            {reply, Data}
    %% Prevent the gen_server from hanging indefinitely in case the
    %% spawned process is taking too long processing the request.
    after get_env(timeout) -> 
            {stop, port_timeout}
    end.

encode({elev_init, elevator}) -> [1, 0];
encode({elev_init, simulator}) -> [1, 1];
encode({elev_set_motor_dir, stop}) -> [2, 1];
encode({elev_set_motor_dir, up}) -> [2, 2];
encode({elev_set_motor_dir, down}) -> [2, 0];
encode({elev_set_door_open_lamp, off}) -> [3, 0];
encode({elev_set_door_open_lamp, on}) -> [3, 1];
encode({elev_get_obstruction_signal}) -> [4];
encode({elev_get_stop_signal}) -> [5];
encode({elev_set_stop_lamp, off}) -> [6, 0];
encode({elev_set_stop_lamp, on}) -> [6, 1];
encode({elev_get_floor_sensor_signal}) -> [7];
encode({elev_set_floor_indicator, Floor}) -> [8, Floor];
encode({elev_get_order, {up, Floor}}) -> [9, 0, Floor];
encode({elev_get_order, {down, Floor}}) -> [9, 1, Floor];
encode({elev_get_order, {internal, Floor}}) -> [9, 2, Floor];
encode({elev_l, up, Floor, on}) -> [10, 0, Floor, 1];
encode({elev_l, up, Floor, off}) -> [10, 0, Floor, 0];
encode({elev_l, down, Floor, on}) -> [10, 1, Floor, 1];
encode({elev_l, down, Floor, off}) -> [10, 1, Floor, 0];
encode({elev_l, internal, Floor, on}) -> [10, 2, Floor, 1];
encode({elev_l, internal, Floor, off}) -> [10, 2, Floor, 0].