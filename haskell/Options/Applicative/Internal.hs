{-# LANGUAGE NoImplicitPrelude #-}
module Options.Applicative.Internal
    ( P
    , MonadP(..)
    , runP
    , runReadM
    , withReadM
    , contextNames
    , NondetT
    , cut
    , (<!>)
    , disamb
    , hoistList
    ) where

import Compiler.Base
import Compiler.Classes
import Compiler.Num
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Bool
import Data.Either
import Data.Eq
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Monoid
import Options.Applicative.Types

-- Compatibility note: upstream Internal also contains completion machinery
-- and parser-option mapping. This module contains its ordinary parse path.
class (Alternative m, MonadPlus m) => MonadP m where
    enterContext :: String -> ParserInfo a -> m ()
    exitContext :: m ()
    getPrefs :: m ParserPrefs
    errorP :: ParseError -> m a
    exitP :: IsCmdStart -> Parser b -> Maybe a -> m a

-- Spell out ReaderT because BAli-Phy does not permit partial application of the Reader synonym.
newtype P a = P (ExceptT ParseError (StateT [Context] (ReaderT ParserPrefs Identity)) a)

instance Functor P where
    fmap f (P m) = P (fmap f m)

instance Applicative P where
    pure x = P (pure x)
    P f <*> P x = P (f <*> x)

instance Alternative P where
    empty = P empty
    P first <|> P second = P (first <|> second)

instance Monad P where
    P m >>= k = P (m >>= (\x -> case k x of P next -> next))

instance MonadPlus P

instance MonadP P where
    enterContext name parser_info = P (lift (modify ((:) (Context name parser_info))))
    exitContext = P (lift (modify (drop 1)))
    getPrefs = P (lift (lift ask))
    errorP = P . throwE
    exitP is_command_start parser = P . maybe
        (throwE (MissingError is_command_start (SomeParser parser)))
        return

contextNames :: [Context] -> [String]
contextNames contexts = reverse [name | Context name _ <- contexts]

runP :: P a -> ParserPrefs -> (Either ParseError a, [Context])
runP (P parser) preferences = runIdentity
    (runReaderT (runStateT (runExceptT parser) []) preferences)

runReadM :: MonadP m => ReadM a -> String -> m a
runReadM (ReadM reader) text = case reader text of
    Left err -> errorP err
    Right x -> return x

withReadM :: (String -> String) -> ReadM a -> ReadM a
withReadM f (ReadM reader) = ReadM (\text -> case reader text of
    Left (ErrorMsg message) -> Left (ErrorMsg (f message))
    result -> result)

data ListStep a rest
    = ListDone
    | ListNext a rest

newtype ListT m a = ListT { stepListT :: m (ListStep a (ListT m a)) }

mapListStep :: (a -> b) -> (x -> y) -> ListStep a x -> ListStep b y
mapListStep _ _ ListDone = ListDone
mapListStep f g (ListNext x rest) = ListNext (f x) (g rest)

instance Monad m => Functor (ListT m) where
    fmap f = ListT . fmap (mapListStep f (fmap f)) . stepListT

instance Monad m => Applicative (ListT m) where
    pure x = ListT (return (ListNext x empty))
    (<*>) = ap

instance Monad m => Monad (ListT m) where
    parser >>= k = ListT (do
        step <- stepListT parser
        case step of
            ListDone -> return ListDone
            ListNext x rest -> stepListT (k x <|> (rest >>= k)))

instance Monad m => Alternative (ListT m) where
    empty = ListT (return ListDone)
    first <|> second = ListT (do
        step <- stepListT first
        case step of
            ListDone -> stepListT second
            ListNext x rest -> return (ListNext x (rest <|> second)))

instance Monad m => MonadPlus (ListT m)

instance MonadTrans ListT where
    lift action = ListT (fmap (\x -> ListNext x empty) action)

takeListT :: Monad m => Int -> ListT m a -> ListT m a
takeListT 0 _ = empty
takeListT count parser = ListT (fmap (mapListStep id (takeListT (count - 1))) (stepListT parser))

runListT :: Monad m => ListT m a -> m [a]
runListT parser = do
    step <- stepListT parser
    case step of
        ListDone -> return []
        ListNext x rest -> fmap ((:) x) (runListT rest)

-- Nondeterministic search with a cut flag shared by the alternatives at one parser step.
newtype NondetT m a = NondetT { runNondetT :: ListT (StateT Bool m) a }

instance Monad m => Functor (NondetT m) where
    fmap f = NondetT . fmap f . runNondetT

instance Monad m => Applicative (NondetT m) where
    pure = NondetT . pure
    NondetT parser_f <*> NondetT parser_x = NondetT (parser_f <*> parser_x)

instance Monad m => Monad (NondetT m) where
    NondetT parser >>= k = NondetT (parser >>= (runNondetT . k))

instance Monad m => Alternative (NondetT m) where
    empty = NondetT empty
    NondetT first <|> NondetT second = NondetT (first <|> second)

instance Monad m => MonadPlus (NondetT m)

instance MonadTrans NondetT where
    lift = NondetT . lift . lift

infixl 3 <!>

-- Try the second branch only if the first branch has not committed through cut.
(<!>) :: Monad m => NondetT m a -> NondetT m a -> NondetT m a
first <!> second = NondetT (runNondetT first <|> do
    committed <- lift get
    guard (not committed)
    runNondetT second)

cut :: Monad m => NondetT m ()
cut = NondetT (lift (put True))

-- Select the first result when ambiguity is allowed, otherwise require exactly one result.
disamb :: Monad m => Bool -> NondetT m a -> m (Maybe a)
disamb allow_ambiguity parser = do
    results <- evalStateT (runListT (takeListT limit (runNondetT parser))) False
    return (case results of
        [x] -> Just x
        _ -> Nothing)
  where
    limit = if allow_ambiguity then 1 else 2

hoistList :: Alternative m => [a] -> m a
hoistList = foldr (\x rest -> pure x <|> rest) empty
