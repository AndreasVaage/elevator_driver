# Elevator_driver

Elevator driver used to control the elevators at the real time lab at NTNU. Implemented using [ports](http://erlang.org/doc/tutorial/c_port.html) to communicate between erlang and c. Inspired by Pete Kazmier´s [Writing An Erlang Port Using OTP Principles](http://www2.erlangcentral.org/wiki/?title=Writing_an_Erlang_Port_using_OTP_Principles), and Kjetil Kjeka´s [driver](https://github.com/kjetilkjeka/Real-time-elevator/blob/master). 

## Usage
The driver includes an elevator poller which requires two callback functions to call when an event occurs: Currently they are:
	environment_controller:event_reached_new_floor(CurrentFloor)
	environment_controller:event_button_pressed({Button, Floor})

## License

See the [LICENSE](LICENSE.md) file for license rights and limitations (MIT).
	