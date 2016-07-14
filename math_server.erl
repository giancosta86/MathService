-module(math_server).

-author("Gianluca Costa").

-description("Basic server with a functional interface").

-include("operations.hrl").

-export([start/0, stop/0]).

-export([add/2, subtract/2, multiply/2, divide/2]).

-export([init/0, loop/0]).


-define(OPERATIONS_TIMEOUT, 3000).


start() ->
  ProcessPid =
    spawn_link(
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
  unregister(?MODULE),
  ok.



add(Left, Right) ->
  binaryOperation(Left, '+', Right).


subtract(Left, Right) ->
  binaryOperation(Left, '-', Right).


multiply(Left, Right) ->
  binaryOperation(Left, '*', Right).


divide(Left, Right) ->
  binaryOperation(Left, '/', Right).


binaryOperation(Left, OperationSymbol, Right) ->
  binaryOperation(Left, OperationSymbol, Right, ?OPERATIONS_TIMEOUT).


binaryOperation(Left, OperationSymbol, Right, Timeout) ->
  RequestId = make_ref(),

  Request = #operationRequest{
    requestId = RequestId,
    operation = OperationSymbol,
    leftOperand = Left,
    rightOperand = Right,
    requestingPid = self()
  },

  ?MODULE ! Request,

  receive
    #operationResponse{requestId = RequestId, result = Result} ->
      {ok, Result};

    Message ->
      io:format("Unexpected client-side message: ~w\n", [Message])

    after Timeout ->
      {error, timeout}
  end.



init() ->
  io:format("Initializing the server...\n"),
  loop().


loop() ->
  receive
    Request = #operationRequest{operation = Operation, leftOperand = LeftOperand, rightOperand = RightOperand} ->
      Result =
        case Operation of
          '+' ->
            LeftOperand + RightOperand;

          '-' ->
            LeftOperand - RightOperand;

          '*' ->
            LeftOperand * RightOperand;

          '/' ->
            LeftOperand / RightOperand
        end,

      Response =
        #operationResponse{
          requestId = Request#operationRequest.requestId,
          result = Result
        },

      Request#operationRequest.requestingPid ! Response,

      ?MODULE:loop();


    stop ->
      io:format("Terminating the server...\n");


    Message ->
      io:format("Unexpected server-side message: ~w\n", [Message]),
      ?MODULE:loop()
  end.
