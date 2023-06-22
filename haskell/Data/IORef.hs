{-# LANGUAGE NoImplicitPrelude #-}
module Data.IORef (IORef,
                   newIORef,
                   readIORef,
                   writeIORef,
                   modifyIORef,
                   modifyIORef',
                   atomicModifyIORef,
                   atomicModifyIORef',
                   atomicWriteIORef
) where

import Compiler.IO
import Compiler.Prim

data IORef a

foreign import bpcall "Prelude:" newIORef :: a -> IO (IORef a)

foreign import bpcall "Prelude:" readIORef :: IORef a -> IO a
-- We need to use the builtin here in order to avoid inlining IORefs.
-- IORefs can change value, which breaks the assumptions of inlining.

foreign import bpcall "Prelude:" writeIORef :: IORef a -> a -> IO ()

foreign import bpcall "Prelude:" modifyIORef :: IORef a -> (a -> a) -> IO ()

foreign import bpcall "Prelude:modifyIORefStrict" modifyIORef' :: IORef a -> (a -> a) -> IO ()

atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef r f = do
  x <- readIORef r
  let (x',y) = f x
  writeIORef r x'
  return y

atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' r f = do
  x <- readIORef r
  let (x',y) = f x
  writeIORef r x'
  x' `seq` y `seq` return y

foreign import bpcall "Prelude:writeIORef" atomicWriteIORef :: IORef a -> a -> IO ()

{-
mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
-}
