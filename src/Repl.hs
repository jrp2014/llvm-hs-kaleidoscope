{-# LANGUAGE OverloadedStrings #-}

import Codegen

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.String
import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as Text
import Foreign.Ptr
import LLVM.AST.AddrSpace
import LLVM.AST.Constant
import LLVM.AST.Float
import LLVM.AST.FloatingPointPredicate hiding (False, True)
import LLVM.AST.Operand
import LLVM.AST.Type as Type
import LLVM.Context
import LLVM.IRBuilder
import LLVM.Module
import LLVM.OrcJIT
import LLVM.Internal.OrcJIT
--import LLVM.OrcJIT.CompileLayer
import LLVM.PassManager
import LLVM.Pretty
import LLVM.Target
import System.IO
import System.IO.Error
import Text.Read (readMaybe)
import qualified LLVM.Relocation as Reloc
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt




foreign import ccall "dynamic" mkFun :: FunPtr (IO Double) -> IO Double

data JITEnv = JITEnv
  { jitEnvContext       :: Context
  , jitExecutionSession :: ExecutionSession
  , jitEnvCompileLayer  :: IRCompileLayer
  , jitDylib            :: JITDylib
  }



main :: IO ()
main = do
  withContext $ \ctx ->
    withHostTargetMachineDefault$ \tm -> do
--    withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.Default $ \tm -> do
      withExecutionSession $ \exSession -> do
        let dylibName = "myDylib"
        dylib <- createJITDylib exSession dylibName
        ol <- createRTDyldObjectLinkingLayer exSession
        il <- createIRCompileLayer exSession ol tm
        let env = JITEnv ctx exSession il dylib
        ast <- runReaderT (buildModuleT "main" repl) env
        return ()


repl :: ModuleBuilderT (ReaderT JITEnv IO) ()
repl = do
  liftIO $ hPutStr stderr "ready> "
  mline <- liftIO $ catchIOError (Just <$> getLine) eofHandler
  case mline of
    Nothing -> return ()
    Just l -> do
      case readMaybe l of
        Nothing ->  liftIO $ hPutStrLn stderr "Couldn't parse"
        Just ast -> do
          -- anon <- isAnonExpr <$> hoist (buildAST ast)

          env <- lift ask
          ctx <- lift $ asks jitEnvContext
          ol <- lift $ asks jitExecutionSession
          il <- lift $ asks jitEnvCompileLayer
          dylib <- lift $ asks jitDylib

          liftIO $ withModuleFromAST ctx ast $ \mdl -> do
            withClonedThreadSafeModule mdl $ \tsm -> do
              addModule tsm dylib il
              let spec = defaultCuratedPassSetSpec { optLevel = Just 3 }
              -- this returns true if the module was modified
              withPassManager spec $ flip runPassManager mdl
              when anon (jit env dylib mdl >>= hPrint stderr)

          when anon (removeDef def)
      repl
  where
    eofHandler e
      | isEOFError e = return Nothing
      | otherwise = ioError e
    isAnonExpr (ConstantOperand (GlobalReference _ "__anon_expr")) = True
    isAnonExpr _ = False

jit :: JITEnv -> JITDylib -> Module -> IO Double
jit JITEnv {jitEnvCompileLayer = compLayer, jitExecutionSession = exSession} dylib mdl =
  do
    symbol <- lookupSymbol exSession compLayer dylib "__anon_expr"
    case symbol of
      Right (JITSymbol fPtr _) -> mkFun (castPtrToFunPtr (wordPtrToPtr fPtr))
      Left (JITSymbolError msg) -> do
        print msg
        return 0

hoist :: Monad m => ModuleBuilder a -> ModuleBuilderT m a
hoist m = ModuleBuilderT $ StateT $
  return . runIdentity . runStateT (unModuleBuilderT m)

{-

initModule :: AST.Module
initModule = emptyModule "my cool jit"


main :: IO AST.Module
main = do
  let ast = runLLVM initModule logic
  rc <- runJIT ast
  return ast
  -}
