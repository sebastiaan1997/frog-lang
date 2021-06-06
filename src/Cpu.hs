{-# LANGUAGE LambdaCase #-}
module Cpu(RegisterType(..), Register(..), Cpu(..), armCortexM0, getRegistersByType) where


    -- | Common register types
    data RegisterType = GeneralPurpose -- GP, can be used for all things, can load and store
        | Data -- Can only store data, cannot be used for maths.
        | StackPointer -- The current pointer for the stack.
        | ProgramCounter -- The program counter
        | LinkRegister -- The return adress 
        deriving(Show, Eq)

    -- | Describes a register.
    data Register = Register{
        registerIndex :: Int, -- The index of the register.
        registerName :: String, -- The name of the register
        size :: Int, -- The size in bits of the register
        registerType :: RegisterType -- The type of the register, as described above.
    }
        deriving(Show, Eq)
    -- | Abstract description of a CPU.
    data Cpu = Cpu{
        cpuName :: String, -- The name of the CPU
        dataSize :: Int, -- The max size of data that fits into a register.
        addrSize :: Int, -- The size of an adress/pointer
        registers :: [ Register ] -- The available registers.
    }

    
    armCortexM0 :: Cpu
    -- | The description of a ARM Cortex M0 CPU
    armCortexM0 = Cpu{
        cpuName="Arm CORTEX-M0", -- The internal name
        dataSize = 32,
        addrSize = 32,
        registers = [
            Register { registerIndex=0,  registerName="R0",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=1,  registerName="R1",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=2,  registerName="R2",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=3,  registerName="R3",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=4,  registerName="R4",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=5,  registerName="R5",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=6,  registerName="R6",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=7,  registerName="R7",  size=32, registerType=GeneralPurpose },
            Register { registerIndex=8,  registerName="R8",  size=32, registerType=Data },
            Register { registerIndex=9,  registerName="R9",  size=32, registerType=Data },
            Register { registerIndex=10, registerName="R10", size=32, registerType=Data },
            Register { registerIndex=11, registerName="R11", size=32, registerType=Data },
            Register { registerIndex=12, registerName="R12", size=32, registerType=Data },
            Register { registerIndex=13, registerName="SP",  size=32, registerType=StackPointer },
            Register { registerIndex=14, registerName="LR",  size=32, registerType=ProgramCounter },
            Register { registerIndex=15, registerName="PC",  size=32, registerType=LinkRegister }
        ]
    }
    getRegistersByType :: RegisterType -> Cpu -> [Register]
    -- | Gets registers of a CPU by type
    getRegistersByType regType Cpu{registers=r} = filter (\case
                Register{registerType=rt} -> rt == regType
                _ -> False) r

    
    countRegisters ::  RegisterType -> Cpu -> Int
    -- | Counts registers by type.
    countRegisters regType cpu = length (getRegistersByType regType cpu)