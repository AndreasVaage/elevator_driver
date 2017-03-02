# Elevator_driver

Erlang elevator driver used to control the elevators at the real time lab at NTNU, used in the [real time project](https://github.com/TTK4145/Project) in TTK4145. Implemented using [ports](http://erlang.org/doc/tutorial/c_port.html) to communicate between erlang and c. Inspired by Pete Kazmier´s [Writing An Erlang Port Using OTP Principles](http://www2.erlangcentral.org/wiki/?title=Writing_an_Erlang_Port_using_OTP_Principles), and Kjetil Kjeka´s [driver](https://github.com/kjetilkjeka/Real-time-elevator/blob/master). 

## Usage
Rebar3: add 

```erlang
{elevator_driver, {git, "https://github.com/AndreasVaage/elevator_driver", {branch, "master"}}} 
```

to your dependency list in the rebar.config file. Then add the elevator_driver to a supervisors child specifications.

```erlang
{name :: atom(),
        {Module :: module(), Start_fun :: function(), Args :: list()},
        permanent, infinity, supervisor, [elevator_driver_sup]}
```
Then in the *Module* you must define the behaviour,

```erlang
-behaviour(elevator_controller).
```
Implement the start function,

```erlang
Start_fun() ->
	elevator_controller:start_elevator(Module :: module(), simulator|elevator).
```
And the callback functions;
The driver includes an elevator poller which requires two callback functions to call when an event occurs, currently they are:

```erlang
event_button_pressed({Button :: atom(), Floor :: integer()}) -> ok.
```

```erlang
event_reached_new_floor(Floor :: integer()) -> ok.
```
They must be defined in the *Module*.

#Example
supervisor:

```erlang 
-module(some_sup).
-behavior(supervisor).
-export([start_link/2]).
-export([init/1]).

start_link(Environment) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Environment).
 
init(Environment) ->
    {ok, {{one_for_all, 1, 2},
          [{elevator_driver,
        	{environment_controller, start_elevator, []},
       		 permanent, infinity, supervisor, [elevator_driver_sup]}
            ]}}.
```

User module:

```erlang
-module(environment_controller).
-behaviour(elevator_controller).
start_elevator() ->
        elevator_controller:start_elevator(?MODULE, simulator).

event_button_pressed(Button) ->
	io:format("Button ~p was pressed~n",[Button]).

event_reached_new_floor(Floor) -> 
	io:format("Floor ~p was reached~n",[Floor]).
```

#Simulator
The driver also includes a [simulator](https://github.com/TTK4145/Project/tree/master/simulator) which must be started separately if the driver are to work in _simulator_ mode. 

- [ ] This should be done automatic

## License

See the [LICENSE](LICENSE.md) file for license rights and limitations (MIT).
	