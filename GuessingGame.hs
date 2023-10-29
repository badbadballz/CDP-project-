{-
Guessing game where a number where a number between 1 and 10 has to be guessed
to win the prize (9 ADA - fees)
The player/guesser needs to have minted a ticket from the MintTicket contract
and include it in the transaction for it to work.
If the guess is correct, the player wins the prize and their ticket is transferred
to the host's wallet where it can be burned.

Improvements
This might not be possible, but to transfer the ticket to the host whether the guess is
correct or not. This would cause the output of the transaction to be different depending
on the outcome, so how would you build a tranaction like that?

Use of reference scripts to minise fees and tranaction size

Parsing the token name of the ticket to get the information of the number chosen, lottery number etc.

Time limit for the script - but this won't do too much at the moment because
the player will still keep his ticket and use it for the next script

Cardano cli commands

Setting the datum

cardano-cli transaction build \
--tx-in $U \
--tx-out "$(addr guessingGame)+9000000" \
--tx-out-inline-datum-file "$DATA_PATH/tenAlice.json" \
--change-address "$(addr alice)" \
--out-file $TX_PATH/guessingGame.raw

Checking the ticket / claiming the prize

T=0b354fb06f47b6d29c528d8ee01cb6e203130264c0c4ff8fb4dd13f5.6ce433d37b665b22cf2a771ebbf33a3a3833e9a7dbe6b06dafe4fb0d73fc3320
U= script utxo
U2= bob's utxo with the token/ticket
S1= bob's key hash
S2= alice's/host's key hash
C= collateral

cardano-cli transaction build \
--tx-in $U \
--tx-in-script-file $PLUTUS_SCRIPTS_PATH/guessingGame.plutus \
--tx-in-inline-datum-present \
--tx-in-redeemer-file $DATA_PATH/tenGuess.json \
--tx-in $U2 \
--tx-out "$(addr alice)+$(min-utxo bob 1 $T)+1 $T" \
--change-address "$(addr bob)" \
--required-signer-hash $S1 \
--required-signer-hash $S2 \
--tx-in-collateral $C \
--out-file $TX_PATH/guessingGame-g.raw

tx-witness guessingGame-g alice bob, tx-assemble guessing-g alice bob etc to sign the transaction

-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Contracts.GuessingGame where

import Contracts.MintTicket qualified as Mint
import Data.Map qualified as Map
import Jambhala.Plutus
import Jambhala.Utils

price :: Integer
price = 10_000_000

secretNumber :: Integer
secretNumber = 34 -- an attempt to make the hash a bit harder to brute force

type Host = PubKeyHash

type Guess = Integer

data HashNumber = HashNumber
  { numberHash :: BuiltinByteString,
    host :: PubKeyHash
  }
  deriving (Generic, ToJSON, FromJSON)

unstableMakeIsData ''HashNumber

--PKH of Alice / host of this contract
{-# INLINEABLE host' #-}
host' :: PubKeyHash
host' = "c7d0b7d80a3c5f0120084c53833c2f227d6f2f28d04f32ce15ce3254"

{-# INLINEABLE makeHash #-}
makeHash :: Integer -> BuiltinByteString
makeHash i = sha2_256 $ serialiseData $ mkI (i #+ secretNumber)

{-# INLINEABLE checkValidTokenName #-}
checkValidTokenName :: [TxInInfo] -> CurrencySymbol -> TokenName -> Bool
checkValidTokenName [] _ _ = False
checkValidTokenName (txi : txis) c t = (checkTokenName txi c t #== True) || checkValidTokenName txis c t

{-# INLINEABLE checkTokenName #-}
checkTokenName :: TxInInfo -> CurrencySymbol -> TokenName -> Bool
checkTokenName (TxInInfo txInInfoResolved TxOut {..}) c t =
  pany (\(c', t', q) -> c' #== c && t' #== t && q #== 1) $ flattenValue txOutValue

{-# INLINEABLE returnGuesserPKH #-}
returnGuesserPKH :: [PubKeyHash] -> Host -> Maybe PubKeyHash
returnGuesserPKH [] _ = Nothing
returnGuesserPKH (p : ps) h = if p #/= h then Just p else returnGuesserPKH ps h

{-# INLINEABLE checkPrizeNumber #-}
checkPrizeNumber :: BuiltinByteString -> Bool
checkPrizeNumber n = pfoldr (\x acc -> (makeHash x #== n) || acc) False [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

{-# INLINEABLE playLambda #-}
playLambda :: Host -> CurrencySymbol -> HashNumber -> Guess -> ScriptContext -> Bool
playLambda h c (HashNumber n h') g (ScriptContext txi@TxInfo {..} _) =
  let gpkh = returnGuesserPKH txInfoSignatories h
   in case gpkh of
        Just gpkh' ->
          let tn = Mint.makeTicketName g gpkh'
           in traceIfFalse "Datum prize number not within 1 - 10" (checkPrizeNumber n)
                && traceIfFalse "Your guess number is incorrect" (n #== makeHash g)
                && traceIfFalse "Not exactly 2 signatories in this transaction" (plength txInfoSignatories #== 2)
                && traceIfFalse "The hosts do not match" (h #== h')
                && traceIfFalse "Not signed by Host" (txSignedBy txi h')
                && traceIfFalse
                  "Ticket has incorrect currency symbol / token name"
                  (checkValidTokenName txInfoInputs c tn)
                && traceIfFalse
                  "Didn't transfer used ticket to host"
                  (Mint.checkPayment h c tn 1 txInfoOutputs)
        Nothing -> traceIfFalse "Can't find the valid PKH of player" False

{-# INLINEABLE untyped #-}
untyped :: Host -> CurrencySymbol -> UntypedValidator
untyped h m = mkUntypedValidator (playLambda h m)

type GuessingGame = ValidatorContract "guessingGame"

type TestMint = MintingContract "testMint"

contract :: Host -> CurrencySymbol -> GuessingGame
contract h m = mkValidatorContract ($$(compile [||untyped||]) `applyCode` liftCode h `applyCode` liftCode m)

mp' :: Mint.MintTicket
mp' = Mint.compileScript host'

mpcs' :: CurrencySymbol
mpcs' = getCurrencySymbol mp'

exports :: JambExports
exports =
  export
    (defExports $ contract host' mpcs')
      { dataExports =
          [ HashNumber {numberHash = makeHash 10, host = host'} `toJSONfile` "tenAlice",
            (1 :: Integer) `toJSONfile` "oneGuess",
            (10 :: Integer) `toJSONfile` "tenGuess",
            (11 :: Integer) `toJSONfile` "elevenGuess"
          ],
        emulatorTest = test
      }

-- Offchain emulator code
{-
Couldn't get the emulator to work after trying out various ways
The issue was how to give an emulator wallet a ticket first or initalising
the emulator with custom tokens and ADA.

-}

-- latest 23/10 try to set a different mode to give tokens to a wallet, how to make a token?

mp :: Mint.MintTicket
mp = Mint.compileScript (pkhForWallet 1)

mpcs :: CurrencySymbol
mpcs = getCurrencySymbol mp

av :: GuessingGame
av = contract (pkhForWallet 1) mpcs

instance ValidatorEndpoints GuessingGame where
  data GiveParam GuessingGame
    = SetNumberHost {getN :: Integer, getH :: PubKeyHash}
    | GiveTicket {getGuess :: Guess}
    deriving (Generic, ToJSON, FromJSON)
  data GrabParam GuessingGame = Guess {guessNumber :: Integer}
    deriving (Generic, ToJSON, FromJSON)

  give :: GiveParam GuessingGame -> ContractM GuessingGame ()
  give (SetNumberHost n h) = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor av,
          constraints = mustPayScriptWithDatum av (HashNumber (makeHash n) h) (lovelaceValueOf 50_000_000)
        }
  give (GiveTicket g) = do
    -- mustMint !?!?
    ownPkh <- getOwnPKH
    let tn = Mint.makeTicketName g ownPkh
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor av, -- lookups might present a problem...
          constraints = mustMint mp (Mint.makeTicketName 1 ownPkh) 1
          --mustPayPKH ownPkh (mkMintingValue mp tn 1)
        }

  --nft example has the key to finishing ? Trying to find the value of the guesser (ownUtxos)
  -- ownPKH

  -- i need the minted token already in the wallet before running this grab call...
  -- can i mint something from another file / contract??? in this emulator???
  -- try importing the testmint.hs and then using that as part of the emulator??

  grab :: GrabParam GuessingGame -> ContractM GuessingGame ()
  grab (Guess g) = do
    ownPkh <- getOwnPKH
    utxos <- getUtxosAt av
    guesserUtxos <- ownUtxos
    let validUtxos = filterByDatum (findPrize g (pkhForWallet 1)) utxos
    -- has to return (Map TxOutRef DecoratedTxOut)
    case Map.toList validUtxos of
      (oref, dtxout) : _ -> do
        let validGuesserUtxos = Map.filter (findTicketName mpcs g ownPkh) guesserUtxos
        if validGuesserUtxos /= mempty
          then
            submitAndConfirm
              Tx
                { lookups = scriptLookupsFor av `andUtxos` validUtxos `andUtxos` validGuesserUtxos,
                  constraints = oref `mustBeSpentWith` g <> mustSign (pkhForWallet 1)
                }
          else logStr "No valid Ticketname!"
      _ -> logStr "No valid datum!"
    where
      findTicketName :: CurrencySymbol -> Integer -> PubKeyHash -> DecoratedTxOut -> Bool
      findTicketName cs g pkh dtxo =
        case getDecoratedTxOutValue dtxo of
          Just v ->
            let tn = Mint.makeTicketName g pkh
             in pany (\(cs', tn', q') -> cs' #== cs && tn' #== tn && q' #== 1) (flattenValue v)
          Nothing -> False
      findPrize :: Guess -> PubKeyHash -> HashNumber -> Bool
      findPrize g pkh hn = (numberHash hn #== makeHash g) && (host hn #== pkh)

test :: EmulatorTest
test =
  initEmulator @GuessingGame
    3
    [ SetNumberHost {getN = 10, getH = pkhForWallet 1} `fromWallet` 1,
      GiveTicket {getGuess = 1} `fromWallet` 1
    ]

--SetNumberHost {getN = 10, getH = (pkhForWallet 1)} `fromWallet` 1

--GiveTicket {getGuess = 1} `fromWallet` 2 --didn't work because wallet 2 doesn't actually have that token...
