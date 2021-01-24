module Result(Result(..), isError, isSuccess, isWarning, isUnimplemented, success, mapSuccess, unwrapResult, mapIOSuccess, unwrapIOResult) where
  data Result a = Success a | Warning (a, String) | Error String | Unimplemented (Maybe String)
    deriving (Show, Eq)


  isError :: Result a -> Bool
  isError (Error _) = True

  isError _ = False

  isSuccess :: Result a -> Bool
  isSuccess (Success _) = True
  isSuccess _ = False

  isWarning :: Result a -> Bool
  isWarning (Warning _) = True
  isWarning _ = False


  isUnimplemented :: Result a -> Bool
  isUnimplemented (Unimplemented _) = True
  isUnimplemented _ = False

  success :: Result a -> a -> a
  success input def = case input of
    (Success value) -> value
    (Warning (value, _)) -> value
    otherwise -> def


  mapSuccess :: (a -> b) -> Result a -> Result b
  mapSuccess resFn (Success value) = Success (resFn value)
  mapSuccess resFn (Warning (value,message)) = Warning(resFn value, message)
  mapSuccess _ (Unimplemented msg) = Unimplemented msg
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
