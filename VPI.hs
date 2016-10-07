{-# LANGUAGE RecordWildCards #-}
module VPI where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Functor
import Data.Int
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Data.Bool
import Numeric

--Task registration

data SysTFType
    = SysTask
    | SysFunc 

sysTFTypeToInt32 SysTask = 1
sysTFTypeToInt32 SysFunc = 2

data VPISystfData = VPISystfData {
    typ         :: SysTFType,
    sysFuncType :: Int32,
    tfName      :: String,
    callTF      :: Ptr CUChar -> IO CInt,
    compileTF   :: Maybe (Ptr CUChar -> IO CInt),
    sizeTF      :: Maybe (Ptr CUChar -> IO CInt),
    userData    :: Ptr CUChar
}

foreign import ccall "wrapper" 
    tfWrap :: (Ptr CUChar -> IO CInt) -> IO (FunPtr (Ptr CUChar -> IO CInt))

instance Storable VPISystfData where
    sizeOf    _                    = 48
    alignment _                    = alignment (0 :: Float)
    peek                           = undefined
    poke      ptr VPISystfData{..} = do

        poke (ptr `plusPtr` 0) (sysTFTypeToInt32 typ :: CInt)
        poke (ptr `plusPtr` 4) (fromIntegral sysFuncType :: CInt)

        tfNamePtr <- newCString tfName
        poke (ptr `plusPtr` 8) tfNamePtr

        callTFPtr <- tfWrap callTF
        poke (ptr `plusPtr` 16) callTFPtr

        compileTFPtr <- maybe (return nullFunPtr) tfWrap compileTF
        poke (ptr `plusPtr` 24) compileTFPtr

        sizeTFPtr <- maybe (return nullFunPtr) tfWrap sizeTF
        poke (ptr `plusPtr` 32) sizeTFPtr

        poke (ptr `plusPtr` 40) userData


data CVPIHandle
newtype VPIHandle = VPIHandle (Ptr CVPIHandle)

nullVPIHandle = VPIHandle nullPtr

foreign import ccall safe "vpi_register_systf"
    c_vpiRegisterTF :: Ptr VPISystfData -> IO (Ptr CVPIHandle)

registerTF :: VPISystfData -> IO VPIHandle
registerTF tf = alloca $ \ptr -> do
    poke ptr tf
    VPIHandle <$> c_vpiRegisterTF ptr

--Object handling

data ObjectCode
    = ConstantObject
    | FunctionObject
    | IntegerVarObject
    | IteratorObject
    | MemoryObject
    | MemoryWordObject
    | ModPathObject
    | ModuleObject
    | NamedBeginObject
    | NamedEventObject
    | NamedForkObject
    | NetObject
    | ParameterObject
    | PartSelectObject
    | PathTermObject
    | PortObject
    | RealVarObject
    | RegObject
    | SysFuncCallObject
    | SysTaskCallObject
    | TaskObject
    | TimeVarObject
    | UdpDefnObject
    | UserSystfObject
    | NetArrayObject
    | IndexObject
    | LeftRangeObject
    | ParentObject
    | RightRangeObject
    | ScopeObject
    | SysTfCallObject
    | ArgumentObject
    | InternalScopeObject
    | ModPathInObject
    | ModPathOutObject
    | VariablesObject
    | ExprObject

objectCodeToInt32 ConstantObject      = 7
objectCodeToInt32 FunctionObject      = 20
objectCodeToInt32 IntegerVarObject    = 25
objectCodeToInt32 IteratorObject      = 27
objectCodeToInt32 MemoryObject        = 29
objectCodeToInt32 MemoryWordObject    = 30
objectCodeToInt32 ModPathObject       = 31
objectCodeToInt32 ModuleObject        = 32
objectCodeToInt32 NamedBeginObject    = 33
objectCodeToInt32 NamedEventObject    = 34
objectCodeToInt32 NamedForkObject     = 35
objectCodeToInt32 NetObject           = 36
objectCodeToInt32 ParameterObject     = 41
objectCodeToInt32 PartSelectObject    = 42
objectCodeToInt32 PathTermObject      = 43
objectCodeToInt32 PortObject          = 44
objectCodeToInt32 RealVarObject       = 47
objectCodeToInt32 RegObject           = 48
objectCodeToInt32 SysFuncCallObject   = 56
objectCodeToInt32 SysTaskCallObject   = 57
objectCodeToInt32 TaskObject          = 59
objectCodeToInt32 TimeVarObject       = 63
objectCodeToInt32 UdpDefnObject       = 66
objectCodeToInt32 UserSystfObject     = 67
objectCodeToInt32 NetArrayObject      = 114
objectCodeToInt32 IndexObject         = 78
objectCodeToInt32 LeftRangeObject     = 79
objectCodeToInt32 ParentObject        = 81
objectCodeToInt32 RightRangeObject    = 83
objectCodeToInt32 ScopeObject         = 84
objectCodeToInt32 SysTfCallObject     = 85
objectCodeToInt32 ArgumentObject      = 89
objectCodeToInt32 InternalScopeObject = 92
objectCodeToInt32 ModPathInObject     = 95
objectCodeToInt32 ModPathOutObject    = 96
objectCodeToInt32 VariablesObject     = 100
objectCodeToInt32 ExprObject          = 102

nothingOnNull :: Ptr a -> Maybe (Ptr a)
nothingOnNull ptr 
    | ptr == nullPtr = Nothing
    | otherwise      = Just ptr

foreign import ccall safe "vpi_iterate"
    c_vpiIterate :: CInt -> Ptr CVPIHandle -> IO (Ptr CVPIHandle)

vpiIterate :: ObjectCode -> VPIHandle -> IO (Maybe VPIHandle)
vpiIterate typ (VPIHandle hdl) = fmap VPIHandle <$> nothingOnNull <$> (c_vpiIterate (objectCodeToInt32 typ) hdl)

foreign import ccall safe "vpi_scan"
    c_vpiScan :: Ptr CVPIHandle -> IO (Ptr CVPIHandle)

vpiScan :: VPIHandle -> IO (Maybe VPIHandle)
vpiScan (VPIHandle hdl) = fmap VPIHandle <$> nothingOnNull <$> c_vpiScan hdl

foreign import ccall safe "vpi_handle_by_index"
    c_vpiHandleByIndex :: Ptr CVPIHandle -> CInt -> IO (Ptr CVPIHandle)

vpiHandleByIndex :: VPIHandle -> Int -> IO (Maybe VPIHandle)
vpiHandleByIndex (VPIHandle ptr) idx = fmap VPIHandle <$> nothingOnNull <$> c_vpiHandleByIndex ptr (fromIntegral idx)

foreign import ccall safe "vpi_handle_by_name"
    c_vpiHandleByName :: CString -> Ptr CVPIHandle -> IO (Ptr CVPIHandle)

--This doesn't seem to actually work. Use getHandle below.
vpiHandleByName :: String -> VPIHandle -> IO (Maybe VPIHandle)
vpiHandleByName str (VPIHandle ptr) = fmap (fmap VPIHandle) $ fmap nothingOnNull $ withCAString str $ \strPtr -> 
    c_vpiHandleByName strPtr ptr

foreign import ccall safe "vpi_get_str"
    c_vpiGetString :: CInt -> Ptr CVPIHandle -> IO CString

data Property
    = UndefinedProperty
    | TypeProperty
    | NameProperty
    | FullNameProperty
    | SizeProperty
    | FileProperty
    | LineNoProperty
    | TopModuleProperty
    | CellInstanceProperty
    | DefNameProperty
    | TimeUnitProperty
    | TimePrecisionProperty
    | DefFileProperty
    | DefLineNoProperty
    | ScalarProperty
    | VectorProperty

propertyToInt32 UndefinedProperty     = -1
propertyToInt32 TypeProperty          = 1
propertyToInt32 NameProperty          = 2
propertyToInt32 FullNameProperty      = 3
propertyToInt32 SizeProperty          = 4
propertyToInt32 FileProperty          = 5
propertyToInt32 LineNoProperty        = 6
propertyToInt32 TopModuleProperty     = 7
propertyToInt32 CellInstanceProperty  = 8
propertyToInt32 DefNameProperty       = 9
propertyToInt32 TimeUnitProperty      = 11
propertyToInt32 TimePrecisionProperty = 12
propertyToInt32 DefFileProperty       = 15
propertyToInt32 DefLineNoProperty     = 16
propertyToInt32 ScalarProperty        = 17
propertyToInt32 VectorProperty        = 18

vpiGetString :: Property -> VPIHandle -> IO String
vpiGetString prop (VPIHandle ref) = do
    res <- c_vpiGetString (propertyToInt32 prop) ref
    peekCString res

--VPI values
data VPIValue
    = VPIBinString String
    | VPIOctString String
    | VPIDecString String
    | VPIHexString String
    | VPIInteger   CInt
    | VPIReal      CFloat
    | VPIString    String

instance Storable VPIValue where
    sizeOf    _                  = 12
    alignment _                  = alignment (0 :: Float)
    peek                         = undefined

    poke      ptr (VPIBinString x) = do
        str <- newCString x --TODO: free it
        poke (ptr `plusPtr` 0) (1 :: CInt)
        poke (ptr `plusPtr` 8) str

    poke      ptr (VPIOctString x) = do
        str <- newCString x --TODO: free it
        poke (ptr `plusPtr` 0) (2 :: CInt)
        poke (ptr `plusPtr` 8) str

    poke      ptr (VPIDecString x) = do
        str <- newCString x --TODO: free it
        poke (ptr `plusPtr` 0) (3 :: CInt)
        poke (ptr `plusPtr` 8) str

    poke      ptr (VPIHexString x) = do
        str <- newCString x --TODO: free it
        poke (ptr `plusPtr` 0) (4 :: CInt)
        poke (ptr `plusPtr` 8) str

    poke      ptr (VPIInteger x) = do
        poke (ptr `plusPtr` 0) (6 :: CInt)
        poke (ptr `plusPtr` 8) x

    poke      ptr (VPIReal x) = do
        poke (ptr `plusPtr` 0) (7 :: CInt)
        poke (ptr `plusPtr` 8) x

    poke      ptr (VPIString x) = do
        str <- newCString x --TODO: free it
        poke (ptr `plusPtr` 0) (8 :: CInt)
        poke (ptr `plusPtr` 8) str

foreign import ccall safe "vpi_put_value"
    c_vpiPutValue :: VPIHandle -> Ptr VPIValue -> Ptr () -> CInt -> IO VPIHandle

data DelayMode
    = NoDelay

delayModeToInt32 NoDelay = 1

vpiPutValue :: VPIHandle -> VPIValue -> IO VPIHandle
vpiPutValue hdl value = alloca $ \ptr -> do
    poke ptr value
    c_vpiPutValue hdl ptr nullPtr (delayModeToInt32 NoDelay)

--Higher level helpers

getHandle :: VPIHandle -> String -> IO (Maybe VPIHandle)
getHandle modH nm = runMaybeT $ findIt modH
    where
    findIt :: VPIHandle -> MaybeT IO VPIHandle
    findIt registerIterator = do
        regHandle <- MaybeT $ vpiScan registerIterator
        thisNm    <- lift   $ vpiGetString NameProperty regHandle
        case thisNm == nm of
            True  -> return regHandle
            False -> findIt registerIterator

boolValue :: Bool -> VPIValue
boolValue = VPIInteger . bool 0 1 

numericValue :: (Integral a, Show a) => a -> VPIValue
numericValue = VPIHexString . flip showHex ""

