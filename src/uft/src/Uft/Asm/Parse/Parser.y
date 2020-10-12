{
-- vim: ft=haskell
{-|
   Module:      Uft.Asm.Parse.Parser
   Description: Happy parser for the Uft assembly language
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Uft.Asm.Parse.Parser
    ( parseProg
    ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Data.ByteString.Internal  (w2c)
import qualified Data.ByteString.Lazy      as Lazy.ByteString
import qualified Data.ByteString.Lazy      as Lazy (ByteString)
import           Data.ByteString.Lazy.Lens (unpackedChars)
import           Data.Loc
import           Data.String.Conversions   (convertString)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Word                 (Word8)
import           GHC.Float                 (double2Int)
import           Text.Read                 (readMaybe)
import           Uft.Asm.Ast
import           Uft.Asm.Parse.Lexer       (lexToken)
import           Uft.Asm.Parse.Monad
import           Uft.Asm.Parse.Tokens      (Token, Token_)
import qualified Uft.Asm.Parse.Tokens      as T
}

%monad { Parser } { (>>=) } { pure }
%lexer { (>>=) lexToken } { L _ T.EOF }
%tokentype { Token }
%error { parseError }

%token
    ABS         { L _ T.Abs         }
    BOOLEANCHK  { L _ T.BooleanChk  }
    CAR         { L _ T.Car         }
    CDR         { L _ T.Cdr         }
    COLON       { L _ T.Colon       }
    COLONEQ     { L _ T.ColonEq     }
    CONS        { L _ T.Cons        }
    DIV         { L _ T.Div         }
    EMPTYLIST   { L _ T.Emptylist   }
    EQ          { L _ T.Eq          }
    ERROR       { L _ T.Error       }
    FALSE       { L _ T.False       }
    FUNCTION    { L _ T.Function    }
    FUNCTIONCHK { L _ T.FunctionChk }
    GT          { L _ T.Gt          }
    GEQ         { L _ T.Geq         }
    GOTO        { L _ T.Goto        }
    HALT        { L _ T.Halt        }
    HASH        { L _ T.Hash        }
    IDIV        { L _ T.IDiv        }
    IF          { L _ T.If          }
    LABEL       { T.PatLabel $$     }
    LBRACE      { L _ T.LBrace      }
    LT          { L _ T.Lt          }
    LEQ         { L _ T.Leq         }
    LPAREN      { L _ T.LParen      }
    MINUS       { L _ T.Minus       }
    MOD         { L _ T.Mod         }
    MUL         { L _ T.Mul         }
    NEWLINE     { L _ T.Newline     }
    NIL         { L _ T.Nil         }
    NILCHK      { L _ T.NilChk      }
    NULLCHK     { L _ T.NullChk     }
    NUMBERCHK   { L _ T.NumberChk   }
    NUM         { T.PatNum $$       }
    PAIRCHK     { L _ T.PairChk     }
    PLUS        { L _ T.Plus        }
    PRINT       { L _ T.Print       }
    PRINTLN     { L _ T.Println     }
    PRINTU      { L _ T.Printu      }
    RBRACE      { L _ T.RBrace      }
    REG         { T.PatReg $$       }
    RPAREN      { L _ T.RParen      }
    SEMICOLON   { L _ T.Semicolon   }
    STR         { T.PatStr $$       }
    SYMBOLCHK   { L _ T.SymbolChk   }
    TRUE        { L _ T.True        }

%name parseProg prog

%%

prog :: { [Instr] }
  : instrs { $1 }

instrs :: { [Instr] }
  : instrs sep1 instr { $1 ++ unLoc $3 }
  | instr { unLoc $1 }
  | { [] }

sep1 :: { () }
  : NEWLINE   msep { () }
  | SEMICOLON msep { () }

msep :: { () }
  : NEWLINE   msep { () }
  | SEMICOLON msep { () }
  | {- empty -}    { () }

instr :: { L [Instr] }
  : LABEL COLON         { L ($1 <--> $2) [Deflabel (unLoc $1)] }
  | LABEL COLON command { L ($1 <--> $3) [Deflabel (unLoc $1), unLoc $3] }
  | command             { L (locOf $1) [unLoc $1] }

command :: { L Instr }
  : GOTO LABEL { L ($1 <--> $2) $ GotoLabel (unLoc $2) }
  | GOTO NUM   { L ($1 <--> $2) $ GotoOffset (round (unLoc $2)) }
  | REG COLONEQ REG binop REG {
      L ($1 <--> $5) $ Cmd $4 (Just (unLoc $1)) [unLoc $3, unLoc $5]
  }
  | REG COLONEQ unaryFun REG {
      L ($1 <--> $4) $ Cmd $3 (Just (unLoc $1)) [unLoc $4]
  }
  | REG COLONEQ CONS REG REG {
      L ($1 <--> $5) $ Cmd Cons (Just (unLoc $1)) [unLoc $4, unLoc $5]
  }
  | REG COLONEQ literal {
      L ($1 <--> $3) $ LoadLit (unLoc $1) (unLoc $3)
  }
  | REG COLONEQ FUNCTION NUM LBRACE instrs RBRACE {%
      let arity = unLoc $4
          arity' = double2Int arity
       in if arity == fromIntegral arity'
             then pure $ L ($1 <--> $7) $ LoadFunc (unLoc $1) arity' $6
             else throwError ("Non-integer arity " <> Text.pack (show arity))
  }
  | HALT { L (locOf $1) $ Cmd Halt Nothing [] }
  | PRINT   REG { L ($1 <--> $2) $ Cmd Print Nothing [unLoc $2] }
  | PRINTU  REG { L ($1 <--> $2) $ Cmd Printu Nothing [unLoc $2] }
  | PRINTLN REG { L ($1 <--> $2) $ Cmd Println Nothing [unLoc $2] }

unaryFun :: { Cmd }
  : CAR         { Car }
  | CDR         { Cdr }
  | ABS         { Abs }
  | FUNCTIONCHK { FunctionChk }
  | PAIRCHK     { PairChk     }
  | SYMBOLCHK   { SymbolChk   }
  | NUMBERCHK   { NumberChk   }
  | BOOLEANCHK  { BooleanChk  }
  | NULLCHK     { NullChk     }
  | NILCHK      { NilChk      }
  | HASH        { Hash }

binop :: { Cmd }
  : PLUS  { Plus  }
  | MINUS { Minus }
  | MUL   { Mul   }
  | MOD   { Mod   }
  | DIV   { Div   }
  | IDIV  { IDiv  }
  | LT    { Lt    }
  | GT    { Gt    }
  | LEQ   { Leq   }
  | GEQ   { Geq   }
  | EQ    { Eq    }

literal :: { L Literal }
  : NUM       { L (locOf $1) $ LitNum (unLoc $1) }
  | STR       { L (locOf $1) $ LitStr (unLoc $1) }
  | TRUE      { L (locOf $1) LitTrue      }
  | FALSE     { L (locOf $1) LitFalse     }
  | EMPTYLIST { L (locOf $1) LitEmptylist }
  | NIL       { L (locOf $1) LitNil       }

{
parseError :: Token -> Parser a
parseError (L loc t) =
    throwError (Text.pack (displayLoc loc) <> ":Error at token " <> Text.pack (show t))
}
