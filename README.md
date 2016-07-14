# MathService

*Basic math service written in Erlang*


## Introduction

MathService is *my very first Erlang project*! :ghost:

I have created it while learning the language and writing [Introduction to Erlang](http://www.slideshare.net/giancosta86/introduction-to-erlang-64035190).

The project consists of the following modules:

* **math_server**: providing:

    * **start/0** and **stop/0** *control functions* for starting/stopping the *math_service* registered process

    * a *functional interface* - **add/2**, **subtract/2**, **multiply/2** and **divide/2** - hiding the actual message-passing protocol as well as the server location.

  Invalid input (for example, *division by 0*) results in a server crash, making the client functions return **timeout** after a default timeout.

* **math_supervisor**: exposes control functions:

    * **start/0**: starts *math_server* (thus enabling the related functional interface) as well as a *supervisor* process that detects crashes of *math_server* process, respawning and re-registering it

    * **stop/0** stops both the supervisor and *math_server*, unregistering the latter


## Testing the system

0. Start **erl** - Erlang's interactive shell - in the project directory

0. Compile them:

    > c(math_server), c(math_supervisor).

0. Start **math_server**:

    > math_server:start().

0. Test the functional interface:

    > math_server:add(4, 5).

0. Make the server crash:

    > math_server:divide(7, 0).

  The server will crash immediately and the function will return **timeout** after a few seconds; subsequent calls to the functional interface will fail.

0. Re-enter the shell - to exit, you can use:

    > q().

0. Start the supervisor:

    > math_supervisor:start().

0. Test the functional interface again:

    > math_server:multiply(90, 5).

0. Make the functional interface crash again:

    > math_server:divide(8, 0).

  the request will still return *timeout*, but the supervisor will spawn a new server.

0. You might also try:

    > math_server:stop().

0. The functional interface should now work:

    > math_server:subtract(90, 8).

0. To stop the service, you need to run:

    > math_supervisor:stop().


## Final considerations

At the time of this writing, I am still learning Erlang and - of course - it takes time and experience to develop one's personal, *elegant* style when creating software in a language; however, I can certainly say that I am once more *very* satified with **Erlang** and **Functional Programming**, which proved to be very effective in the domain of Distributed Systems, too! ^\_\_^


## Further references

* [Introduction to Erlang](http://www.slideshare.net/giancosta86/introduction-to-erlang-64035190)

* [Erlang](http://www.erlang.org/)

* [Erlang Programming](http://shop.oreilly.com/product/9780596518189.do)

* [Learn You Some Erlang for Great Good!](learnyousomeerlang.com)
