# Elevator_driver

Erlang elevator driver used to control the elevators at the real time lab at NTNU, used in the [real time project](https://github.com/TTK4145/Project) in TTK4145. Implemented using [ports](http://erlang.org/doc/tutorial/c_port.html) to communicate between erlang and c. Inspired by Pete Kazmier´s [Writing An Erlang Port Using OTP Principles](http://www2.erlangcentral.org/wiki/?title=Writing_an_Erlang_Port_using_OTP_Principles), and Kjetil Kjeka´s [driver](https://github.com/kjetilkjeka/Real-time-elevator/blob/master). 

Hope this can let you focus on the fun parts of Erlang, and make the project better. If you find errors or make improvements it would be very nice if you sent a pull request, created a issue or made a new open source project. Such that we end up with an awesome driver and thus removes any doubt of which programming language fits this project the best.    

## Download
##### Rebar3 
Not very well tested, but should work: add 

```erlang
{elevator_driver, {git, "https://github.com/AndreasVaage/elevator_driver", {branch, "master"}}} 
```

to your dependency list in the rebar.config file. You could also create a [_checkout folder](https://www.rebar3.org/docs/dependencies).

##### Simple
Copy [elevator_driver.erl](src/elevator_driver.erl)  and the c_src folder to your project.

## Usage
Compile the c sources and make sure the path to the executable is correct such that *elevator_driver.erl* manages to spawn the executable *elevator*. Line 90 in *elevator_driver.erl*. 

Then you can add the elevator_driver to a supervisors child specifications.

```erlang
{name :: atom(),
        {Module :: module(), Start_fun :: function(), Args :: list()},
        permanent, infinity|integer(), worker, [elevator_driver]}
```
Then in the *Module* you can define the behaviour.

```erlang
-behaviour(elevator_driver).
```

You must however implement the callback functions;
The driver requires two callback functions to call when an event occurs, currently they are:

```erlang
event_button_pressed({up|down|internal, Floor :: integer()}) -> ok.
```

```erlang
event_reached_new_floor(Floor :: integer() | the_void) -> ok.
```
They must be defined in the *Module* passed as the first argument in the *start_link* function.

### Functions

```erlang
set_motor_dir(up|down|stop) -> 0.
set_button_light(up|down|internal, Floor :: integer(), on|off) -> 0.
set_door_light(on|off) -> 0.
set_floor_indicator(Floor :: integer()) -> 0.
set_stop_light(on|off) -> 0.
get_floor() -> Floor :: integer() | the_void.

start_link(Module :: module(), simulator|elevator|Configs).

    Configs = [Config, ...].
    Config = 
    {elevator_type, elevator|simulator} | % No default
    {top_floor, integer()} |              % Default 3
    {poll_period, time()} |               % Default 50 (ms)
    {external_program, string()} |        % Default "elevator"
    {external_timeout, time() } |         % Default 3000 (ms)
    {simulator_ip, tuple() } |            % Default {127,0,0,1}
    {simulator_port, integer() }          % Default 15657.
```

### Example
supervisor:

```erlang 
-module(some_sup).
-behavior(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
 
init([]) ->
    {ok, {{one_for_all, 1, 2},
          [{elevator_driver,
        	{environment_controller, start_elevator, []},
       		 permanent, 5000, worker, [elevator_driver]}
            ]}}.
```

User module:

```erlang
-module(environment_controller).
-behaviour(elevator_driver).

start_elevator() ->
        elevator_driver:start_link(?MODULE, simulator).

event_button_pressed({ButtonType, Floor}) ->
	elevator_driver:set_button_light(ButtonType, Floor, on).

event_reached_new_floor(Floor) -> 
	elevator_driver:set_floor_indicator(Floor).
```

### Simulator
The driver can use the [simulator](https://github.com/TTK4145/Project/tree/master/simulator), but then the simulator must be running at the IP and port specified by the *Configs* in the *start_link* function. If none are specified it uses the default values 127.0.0.1 and 15657.  

## License

See the [LICENSE](LICENSE.md) file for license rights and limitations (MIT).
	