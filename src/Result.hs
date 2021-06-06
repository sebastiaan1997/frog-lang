module Result(Result(..), isError, isSuccess, isWarning, isUnimplemented, success, mapSuccess, unwrapResult, mapIOSuccess, unwrapIOResult) where
  data Result a = Success a | Warning (a, String) | Error String | Unimplemented (Maybe String)
    deriving (Show, Eq)

  -- | Checks wether a result is an error.
  isError :: Result a -> Bool
  -- | Returns true if the value is an error.
  isError (Error _) = True 
  -- | Returns false if the value is not an error.
  isError _ = False
  -- | Checks wether a result is succes.
  isSuccess :: Result a -> Bool
  -- | Returns true if the result is success.
  isSuccess (Success _) = True
  -- | Returns false if the result is not success.
  isSuccess _ = False
  -- | Checks wether a result is a warning.
  isWarning :: Result a -> Bool
  -- | Returns true if the result is a warning.
  isWarning (Warning _) = True
  -- | Returns false if the result is not a warning.
  isWarning _ = False

  -- | Checks wether the result is unimplemented
  isUnimplemented :: Result a -> Bool
  -- | Returns true if the result is unimplemented.
  isUnimplemented (Unimplemented _) = True
  -- | Returns false if the result is not unimplemente
  isUnimplemented _ = False
  -- | Returns value of a result if the state is Success or warning.
  success :: Result a -> a -> a
  success input def = case input of
    (Success value) -> value
    (Warning (value, _)) -> value
    otherwise -> def

  -- | Maps succes to a function
  mapSuccess :: (a -> b) -> Result a -> Result b
  -- | If the value is success, execute function and wrap in a success state.
  mapSuccess resFn (Success value) = Success (resFn value)
  -- | If the value is warning, execute function and wrap in a warning state.
  mapSuccess resFn (Warning (value,message)) = Warning(resFn value, message)
  -- | If unimplemented, then return unimplemented
  mapSuccess _ (Unimplemented msg) = Unimplemented msg
  -- | If error, then error.
  mapSuccess _ (Error msg) = Error msg

  mapIOSuccess :: (a -> IO b) -> Result a -> IO (Result b)
  mapIOSuccess resFn (Success value) = do {
      result <- resFn value;
      return (Success result)
  }
  mapIOSuccess resFn (Warning (value,message)) = do {
    result <- resFn value;
    return (Warning(result, message))
  }
  mapIOSuccess _ (Unimplemented msg) = return (Unimplemented msg)
  mapIOSuccess _ (Error msg) = return (Error msg)



  unwrapResult :: Result (Result a) -> Result a
  unwrapResult (Success(innerValue)) = innerValue
  unwrapResult (Warning(innerValue, message)) = case innerValue of
    (Success value) -> Warning(value, message)
    (Warning (value, innerMessage)) -> Warning(value, innerMessage ++ ('\n': message ))
    (Error innerMessage) -> Error innerMessage
    (Unimplemented msg) -> Unimplemented msg
  unwrapResult (Error msg) = Error msg
  unwrapResult (Unimplemented msg) = Unimplemented msg

  unwrapIOResult :: IO (Result (Result a)) -> IO (Result a)
  unwrapIOResult value = do { v <-  value; return (unwrapResult v) }
