{-# LANGUAGE RecordWildCards #-}

module InteractiveGtkMonad (
  module InteractiveGtkMonad
) where

data InteractiveGtkM s a = InteractiveGtkM
  (InteractiveGtkState s -> IO (a, InteractiveGtkState s))

data InteractiveGtkState s = InteractiveGtkState{
  -- gtkApplicationState :: ??,
  userState :: s
}

instance Functor (InteractiveGtkM s) where
  fmap f (InteractiveGtkM g) = InteractiveGtkM h
    where
      h st0 = do
        (val,st1) <- g st0
        return (f val, st1)


instance Applicative (InteractiveGtkM s) where
  pure k = InteractiveGtkM (\s -> return (k, s))

  InteractiveGtkM ff <*> InteractiveGtkM gg = InteractiveGtkM hh
    where
      hh st0 = do
        (fun,st1) <- ff st0
        (val,st2) <- gg st1
        return (fun val,st2)

instance Monad (InteractiveGtkM s) where
  InteractiveGtkM f >>= stg = InteractiveGtkM h
    where
      h st0 = do
        (val1,st1) <- f st0
        st2 <- waitForUserToGo st1
        let InteractiveGtkM g = stg val1
        g st2

  return k = InteractiveGtkM (\s -> return (k, s))

waitForUserToGo :: s -> IO s
waitForUserToGo = return

getInteractiveGtkM :: InteractiveGtkM s s
getInteractiveGtkM = InteractiveGtkM (\s-> return (userState s,s))

setInteractiveGtkM :: s -> InteractiveGtkM s ()
setInteractiveGtkM s = modifyInteractiveGtkM (\_-> s)

modifyInteractiveGtkM :: (s -> s) -> InteractiveGtkM s ()
modifyInteractiveGtkM f = InteractiveGtkM
  (\st-> return ((),st{userState=f $ userState st}))


runInteractiveGtkM :: s -> InteractiveGtkM s a -> IO (a,s)
runInteractiveGtkM s (InteractiveGtkM f) = do
  (val,st) <- f InteractiveGtkState{
    --- set up gtkApplicationState
    userState=s}
  return $ (val,userState st)
