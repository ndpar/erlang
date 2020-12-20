# Generic State Machine

This example demonstrates interaction between two state machines.
It also shows how to use the following features of `gen_statem`

- sending events to itself
- state timeouts
- postponed events
- reply actions

To run the demo, execute these commands in Erlang shell:

    barber:open_shop().
    barber:new_customer(customer:start()).
    barber:new_customer(customer:start()).
    barber:new_customer(customer:start()).
    barber:new_customer(customer:start()).

For the background information and the previous implementation, see this
[blog post](https://blog.ndpar.com/2013/06/11/sleeping-barber-in-erlang/).