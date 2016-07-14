-module(math_supervisor).

-author("Gianluca Costa").

-description("Basic supervisor").

-export([start/0, stop/0]).

-export([init/0, loop/0]).


start() ->
  ProcessPid =
    spawn(
      ?MODULE,
      init,
      []
    ),

  register(
    ?MODULE,
    ProcessPid
  ),

  ProcessPid.


stop() ->
  ?MODULE ! stop,
  ok.


init() ->
  io:format("Initializing the supervisor...\n"),
  process_flag(trap_exit, true),
  math_server:start(),
  loop().



loop() ->
  receive
    {'EXIT', _Pid, _Reason} ->
      io:format("It seems the server has stopped. I'm restarting it...\n"),
      math_server:start(),
      ?MODULE:loop();

    stop ->
      math_server: stop(),
      io:format("Terminating the supervisor...\n");

    Message ->
      io:format("Unexpected supervisor-side message: ~w\n", [Message]),
      ?MODULE:loop()
  end.
