module Disassembly (disassemble) where
import StackMachine (StackProgram)

disassemble :: StackProgram -> String
disassemble = unlines . map show
