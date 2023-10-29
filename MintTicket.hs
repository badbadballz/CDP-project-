{-
Mints one token representing the ticket purchased for one ADA
to interact with the guessing game contract
Alice is the host of this contract.

-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Contracts.MintTicket where

import Data.List (drop)
import Jambhala.Plutus
import Jambhala.Utils
import Plutus.V1.Ledger.Value qualified as PV
import PlutusTx.Builtins.Internal qualified as Ptxbi

guessPrice :: Integer
guessPrice = 1_000_000

type HostPKH = PubKeyHash

type Price = Integer

type PolicyDatum = CurrencySymbol

data Mode = Mint' | Burn'
  deriving (Generic, FromJSON, ToJSON)

makeIsDataIndexed ''Mode [('Mint', 0), ('Burn', 1)]

newtype Guess = Guess {getNumber :: Integer}
  deriving (Generic, FromJSON, ToJSON)

unstableMakeIsData ''Guess

data GuessMode = GuessMode {getNumber' :: Integer, getMode :: Mode}
  deriving (Generic, FromJSON, ToJSON)

unstableMakeIsData ''GuessMode

-- PKH of Alice / Host of the minting contract
{-# INLINEABLE host #-}
host :: PubKeyHash
host = "c7d0b7d80a3c5f0120084c53833c2f227d6f2f28d04f32ce15ce3254"

checkPayment :: PubKeyHash -> CurrencySymbol -> TokenName -> Price -> [TxOut] -> Bool
checkPayment pkh cs tn price = pany (isValidOutput pkh cs tn price)
{-# INLINEABLE checkPayment #-}

isValidOutput :: PubKeyHash -> CurrencySymbol -> TokenName -> Price -> TxOut -> Bool
isValidOutput pkh cs tn price TxOut {..} = case toPubKeyHash txOutAddress of
  Nothing -> False
  Just pkh' -> pkh' #== pkh && valueOf txOutValue cs tn #== price
{-# INLINEABLE isValidOutput #-}

{-# INLINEABLE checkMint #-}
checkMint :: TokenName -> Value -> Bool
checkMint tn minted = pany (\(_, tn', q) -> tn' #== tn && q #== 1) $ flattenValue minted

{-# INLINEABLE makeTicketName #-}
makeTicketName :: Integer -> PubKeyHash -> TokenName
makeTicketName g pkh = TokenName $ sha2_256 $ appendByteString (serialiseData $ mkI g) (getPubKeyHash pkh)

{-# INLINEABLE ticketLambda #-}
ticketLambda :: PubKeyHash -> GuessMode -> ScriptContext -> Bool
ticketLambda pkh (GuessMode g Mint') (ScriptContext TxInfo {..} _) =
  let (s1 : _) = txInfoSignatories
   in traceIfFalse "Guessed/ticket number not between 1 and 10" (1 #<= g && g #<= 10)
        && traceIfFalse "More than one signatory in this transaction" (plength txInfoSignatories #== 1)
        && traceIfFalse "Didn't pay host the ticket price" (checkPayment pkh adaSymbol adaToken guessPrice txInfoOutputs)
        && traceIfFalse "Ticket mint (currencysymbol / token name) invalid" (checkMint (makeTicketName g s1) txInfoMint)
ticketLambda _ (GuessMode _ Burn') (ScriptContext TxInfo {..} _) = True

{-# INLINEABLE untypedLambda #-}
untypedLambda :: PubKeyHash -> UntypedMintingPolicy
untypedLambda = mkUntypedMintingPolicy . ticketLambda

type MintTicket = MintingContract "mintTicket"

compileScript :: PubKeyHash -> MintTicket
compileScript pkh = mkMintingContract ($$(compile [||untypedLambda||]) `applyCode` liftCode pkh)

exports :: JambExports
exports =
  export
    (defExports $ compileScript host)
      { dataExports =
          [ (GuessMode {getNumber' = 1, getMode = Mint'}) `toJSONfile` "mintOne",
            (GuessMode {getNumber' = 10, getMode = Mint'}) `toJSONfile` "mintTen",
            (GuessMode {getNumber' = 11, getMode = Mint'}) `toJSONfile` "mintEleven",
            (GuessMode {getNumber' = 10, getMode = Burn'}) `toJSONfile` "burnTen",
            (GuessMode {getNumber' = 1, getMode = Burn'}) `toJSONfile` "burnOne"
          ],
        emulatorTest = test
      }

instance MintingEndpoint MintTicket where
  data MintParam MintTicket
    = Mint !Integer
    | Burn !Integer
    deriving (Generic, FromJSON, ToJSON)

  mint :: MintParam MintTicket -> ContractM MintTicket ()
  mint mp = do
    ownPkh <- getOwnPKH
    let pkh = pkhForWallet 1
        as = compileScript pkh
        mint' :: Integer -> ContractM MintTicket ()
        mint' g = do
          let ticketName = makeTicketName g ownPkh
          submitAndConfirm
            Tx
              { lookups = scriptLookupsFor as,
                constraints =
                  mustMintWithRedeemer as (GuessMode g Mint') ticketName 1
                    <> mustPayPKH pkh (lovelaceValueOf guessPrice)
                    <> mustSign ownPkh
              }
          logStr $ printf "Minted 1 %s" (PV.toString ticketName)
        burn' :: Integer -> ContractM MintTicket ()
        burn' g = do
          let ticketName = makeTicketName g ownPkh
          submitAndConfirm
            Tx
              { lookups = scriptLookupsFor as,
                constraints = mustMintWithRedeemer as (GuessMode g Burn') ticketName (-1)
              }
          logStr $ printf "Burned -1 %s" (PV.toString ticketName)
    case mp of
      Mint g -> mint' g
      Burn g -> burn' g

test :: EmulatorTest
test =
  initEmulator @MintTicket
    3
    [ Mint 1 `forWallet` 2,
      Mint 9 `forWallet` 2,
      Mint 10 `forWallet` 2,
      Mint 19 `forWallet` 2,
      Mint 32 `forWallet` 2,
      Mint 2000 `forWallet` 2,
      Mint 1 `forWallet` 3,
      Mint 2 `forWallet` 3,
      Mint 3 `forWallet` 3,
      Burn 1 `forWallet` 2,
      Burn 2 `forWallet` 3,
      Burn 2000 `forWallet` 2
    ]

testByteStringtoInt :: BuiltinByteString -> Ptxbi.BuiltinInteger
testByteStringtoInt = Ptxbi.unsafeDataAsI . Ptxbi.mkB

tokenNameToString :: TokenName -> String
tokenNameToString = drop 2 . (PV.toString)

{-

Mints one ticket for a number guess of 10, paying 1 ADA to Alice (Host)
T=0b354fb06f47b6d29c528d8ee01cb6e203130264c0c4ff8fb4dd13f5.6ce433d37b665b22cf2a771ebbf33a3a3833e9a7dbe6b06dafe4fb0d73fc3320
S1= bob's key hash

cardano-cli transaction build \
--tx-in $U \
--mint-script-file $PLUTUS_SCRIPTS_PATH/mintTicket.plutus \
--mint "1 $T" \
--mint-redeemer-file $DATA_PATH/mintTen.json \
--required-signer-hash $S1 \
--tx-out "$(addr alice)+1000000" \
--tx-out "$(addr bob)+$(min-utxo bob 1 $T)+1 $T" \
--change-address "$(addr bob)" \
--tx-in-collateral $C \
--out-file $TX_PATH/mintTicket.raw

Burns the ticket, U2 is UTXOs to pay the fee

cardano-cli transaction build \
--tx-in $U \
--tx-in $U2 \
--mint-script-file $PLUTUS_SCRIPTS_PATH/mintTicket.plutus \
--mint "-1 $T" \
--mint-redeemer-file $DATA_PATH/burnTen.json \
--change-address "$(addr alice)" \
--tx-in-collateral $U2 \
--out-file $TX_PATH/burnTicket.raw

-}