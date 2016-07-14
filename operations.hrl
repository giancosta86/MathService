-record(operationRequest, {
  requestId,
  operation,
  leftOperand,
  rightOperand,
  requestingPid
}).


-record(operationResponse, {
  requestId,
  result
}).
