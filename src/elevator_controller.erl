-module(elevator_controller).
  
%% application callbacks
-export([start_elevator/2, start_elevator/1]).

-export([
		 set_motor_dir/1,        %up/down/stop
         set_button_light/3,     %up/down/internal , int , on/off
         set_door_light/1,       %on/off
         set_floor_indicator/1,  % int
         set_stop_light/1        % on/off
	]).

-callback event_button_pressed({Button :: atom(), Floor :: integer()}) -> ok.
-callback event_reached_new_floor(Floor :: integer()) -> ok.
-spec set_motor_dir(up|down|stop) -> ok.
-spec set_button_light(up|down|internal, Floor :: integer(), on|off) -> ok.
-spec set_door_light(on|off) -> ok.
-spec set_floor_indicator(Floor :: integer()) -> ok.
-spec set_stop_light(on|off) -> ok.


start_elevator(Module, Environment) -> 
	application:set_env(elevator_driver, environment, Environment),
	start_elevator(Module).
 
start_elevator(Module) ->
	application:set_env(elevator_driver, callback_module, Module),
	{ok, Environment} = application:get_env(elevator_driver, environment),
    PrivDir = code:priv_dir(elevator_driver),
    {ok, ExtProg} = application:get_env(elevator_driver, extprog),
    ExtProgWithPath = filename:join([PrivDir, ExtProg]),
    elevator_driver_sup:start_link(ExtProgWithPath, Environment).

 
set_motor_dir(Direction)->
	elevator_driver:set_motor_dir(Direction).
set_button_light(ButtonType, Floor, Value) ->
	elevator_driver:set_button_light(ButtonType, Floor, Value).
set_door_light(Value) ->
	elevator_driver:set_button_light(Value).
set_floor_indicator(Floor) ->
	elevator_driver:set_floor_indicator(Floor).
set_stop_light(Value) ->
	elevator_driver:set_stop_light(Value).
