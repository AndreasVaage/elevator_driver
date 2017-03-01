# Elevator_driver

Erlang elevator driver used to control the elevators at the real time lab at NTNU, used in the [real time project](https://github.com/TTK4145/Project) in TTK4145. Implemented using [ports](http://erlang.org/doc/tutorial/c_port.html) to communicate between erlang and c. Inspired by Pete Kazmier´s [Writing An Erlang Port Using OTP Principles](http://www2.erlangcentral.org/wiki/?title=Writing_an_Erlang_Port_using_OTP_Principles), and Kjetil Kjeka´s [driver](https://github.com/kjetilkjeka/Real-time-elevator/blob/master). 

## Usage
Rebar3: add 

```erlang
{elevator_driver, {git, "https://github.com/AndreasVaage/elevator_driver", {branch, "master"}}} 
```

to your dependency list in the rebar.config file.
The driver includes an elevator poller which requires two callback functions to call when an event occurs, currently they are:

```erlang
event_button_pressed({Button :: atom(), Floor :: integer()}) -> ok.
```

```erlang
event_reached_new_floor(Floor :: integer()) -> ok.
```
The module which implements these callback functions must be specified as the *callback_module* in the elevator_driver.app.src env-list. Edit the file directly or use:

```erlang
application:set_env(elevator_driver, callback_module, YourModule :: atom()).
```

The driver also includes a [simulator](simulator/README.md) which must be started separately if the driver are to work in _simulator_ mode. 

- [ ] This should be done automatic

## License

See the [LICENSE](LICENSE.md) file for license rights and limitations (MIT).
	