{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Simple Instrument Calibration Service Charges Payment Portal Smart Contract

module InstrumentCalibrationServiceChargesPaymentFilter where

-- Import Dependency Data Environment 
import           Control.Monad        (void)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map (Map)
import           Data.Map                  as Map
import           Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8     as C
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)

-- Import Dependency Plutus Environment 
import qualified PlutusTx                  as PlutusTx
import           PlutusTx.Prelude 
import           Plutus.Contract
import PlutusTx.Prelude hiding (pure, (<$>))
import Prelude qualified as Haskell

-- Import Dependency Ledger Environment 
import           Ledger                    (Address, Validator, ScriptContext, Value, scriptAddress, getCardanoTxId, ChainIndexTxOut(..), Datum (Datum), dataHash, Datum (..), DatumHash (..), PaymentPubKeyHash, TxInfo, scriptContextTxInfo, txSignedBy,unPaymentPubKeyHash)
import           Ledger                    hiding (singleton)
import           Ledger.Tx (ChainIndexTxOut (..))
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada

-- Import Dependency Playground Environment 
import           Playground.Contract
import qualified Prelude
import qualified Prelude              as P
import Prelude (String)
import           Text.Printf          (printf)

-- Functor Definition
newtype HashedString = HashedString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''HashedString


-----------------------------------------------------------------------------
-- | Instrument Calibration Service Charges Payment Validation On-Chain Code
-----------------------------------------------------------------------------
minLovelace :: Integer
minLovelace = 2000000

-- | Instrument Calibration Specification Target Datum On-Chain Code
data CalServiceDatum = CalServiceDatum
    { service_beneficiary       :: !PaymentPubKeyHash           -- Public Hash Key Service Provider Beneficiary Address
    , set_ZeroCal               :: !Integer                     -- Baseline Instrument Zero Calibration Setting
    , set_SpanCal               :: !Integer                     -- Baseline Instrument Span Calibration Setting
    , set_Acc                   :: !Integer                     -- Target Instrument Accuracy Calibration Setting
    , set_Rep                   :: !Integer                     -- Target Instrument Repeatability Calibration Setting
    , set_ServiceChargesAmount  :: !Integer                     -- Instrument Calibration Service Charges Amount 
    , set_pin                   :: !Integer                     -- Pin Reward Service Charges Withdrawl Authorization 
    } deriving Show

         
PlutusTx.unstableMakeIsData ''CalServiceDatum
PlutusTx.makeLift ''CalServiceDatum

-- | Instrument Calibration Validation Report Redeemer On-Chain Code
data CalServiceChargesRedeemer = CalServiceChargesRedeemer
    { val_ZeroCal         :: !Integer			-- Validated Value of Instrument Zero Calibration
    , val_SpanCal         :: !Integer			-- Validated Value 0f Instrument Span Calibration Setting
    , val_AccCal          :: !Integer			-- Validated Value 0f Instrument Accuracy Calibration Setting
    , val_RepCal          :: !Integer			-- Validated Value 0f Instrument Repeatability Calibration Setting
    , val_pin             :: !Integer			-- Validated Reward Service Charges Withdrawl Authorization 
    } deriving Show


PlutusTx.unstableMakeIsData ''CalServiceChargesRedeemer
PlutusTx.makeLift ''CalServiceChargesRedeemer


-- | Instrument Calibration Service Charges Payment Validation On-Chain Code
-- Transaction Validation to released Instrument Calibration Service Charges Payment through Instrument Calibration On Spesification Requirements Zero-Span-Accuracy-Repeatability,
-- beneficiarry address matching, and Service Charges Withdrawl Authorization authorization matching to released the Service Charges Amount

{-# INLINABLE validateCCEmission #-}
validateCCEmission :: CalServiceDatum -> CalServiceChargesRedeemer -> ScriptContext -> Bool
validateCCEmission (CalServiceDatum phk sz ss sa sr sca spnt) (CalServiceChargesRedeemer vz vs va vr vpnt) ctx = 
                                                                        traceIfFalse "Calibration Accuracy & Repeatability Performance Not Achieved - Zero Span Adjustment Required" $  sz == vz  && ss == vs && sa >= va  && sr >= vr  &&
                                                                        (traceIfFalse "Wrong Service Charges Withdrawl Authorization!" $ spnt == vpnt) &&
                                                                         traceIfFalse "Service Provider Beneficiary's Signature Not Matched" signedByBeneficiary
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ phk


-- | Instrument Calibration Services Datum and Redeemer Parameters
data CalServiceRewards
instance Scripts.ValidatorTypes CalServiceRewards where
    type instance DatumType CalServiceRewards = CalServiceDatum
    type instance RedeemerType CalServiceRewards = CalServiceChargesRedeemer

-- | The Script Instance to compile validator (ready to go onto the chain)
calServiceRewardsInstance :: Scripts.TypedValidator CalServiceRewards
calServiceRewardsInstance = Scripts.mkTypedValidator @CalServiceRewards
  $$(PlutusTx.compile [|| validateCCEmission ||])
  $$(PlutusTx.compile [|| wrap ||])
    where
      wrap = Scripts.wrapValidator @CalServiceDatum @CalServiceChargesRedeemer


------------------------------------------------------------------------------
-- | Instrument Calibration Service Charges Payment  Validation Off-Chain Code
------------------------------------------------------------------------------

-- | The Address of the Instrument Calibration Service Charges Payment 
calServiceRewardsAddress :: Address
calServiceRewardsAddress = Ledger.scriptAddress (Scripts.validatorScript calServiceRewardsInstance)

-- | Parameters for the "Instrument Calibration Service On Spesification Target" endpoint
data CalSpecParams = CalSpecParams
    { serviceBeneficiary    :: !PaymentPubKeyHash          -- Public Hash Key Service Provider Beneficiary Address
    , zeroCal               :: !Integer                    -- Baseline Instrument Zero Calibration Setting
    , spanCal               :: !Integer                    -- Baseline Instrument Span Calibration Setting           
    , accuracy              :: !Integer                    -- Target Instrument Accuracy Calibration Setting
    , repeatability         :: !Integer                    -- Target Instrument Repeatability Calibration Setting
    , serviceChargesAmount  :: !Integer                    -- Instrument Calibration Service Charges Amount              
    , pin                   :: !Integer                    -- Pin Reward Service Charges Withdrawl Authorization 
    }
    deriving (Generic, ToJSON, FromJSON, ToSchema)


--  | Parameters for the "Instrument Calibration Service Validation Report" endpoint
data CalSpecValParams = CalSpecValParams
    { val_Zero_Calibration           :: !Integer    -- Validated Value of Current Year Carbon Emission in Tonnage
    , val_Span_Calibration           :: !Integer    -- Validated Value 0f Current Year Emission Reduction Factor
    , val_Acc_Calibration            :: !Integer    -- Validated Value 0f Current Year Emission Reduction Factor
    , val_Rep_Calibration            :: !Integer    -- Validated Value 0f Current Year Emission Reduction Factor
    , pin_validation                 :: !Integer    -- Validated Pin Reward Withdrawl Authorization
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)


-- | The schema of the contract, with one endpoint to publish the problem with a Instrument Calibration Services Payment 
--   and submit Instrument Calibration Service SpesificationValidation Value
type CalServiceRewardsSchema =
            Endpoint "Instrument Calibration Service On Spesification Target and Service Charges Funds" CalSpecParams    
        .\/ Endpoint "Instrument Calibration Service Validation" CalSpecValParams

-- | The "Instrument Calibration Service On Spesification Target" contract endpoint.
calsvcrewards :: AsContractError e => CalSpecParams -> Contract () CalServiceRewardsSchema e ()
calsvcrewards (CalSpecParams bnf basezero basespan baseacc baserep svcAmt pnt ) = do
    let datDatum = CalServiceDatum
                { service_beneficiary       = bnf               -- Calibration Datum Public Hash Key Service Provider Beneficiary Address
                , set_ZeroCal               = basezero          -- Calibration Specification Target Datum Baseline Instrument Zero Calibration Setting
                , set_SpanCal               = basespan          -- Calibration Specification Target Datum Baseline Instrument Span Calibration Setting 
                , set_Acc                   = baseacc           -- Calibration Specification Target Datum Baseline Instrument Accuracy Calibration Setting       
                , set_Rep                   = baserep           -- Calibration Specification Target Datum Baseline Instrument Repeatability Calibration Setting
                , set_ServiceChargesAmount  = svcAmt            -- Calibration Datum Instrument Calibration Service Charges Amount in ADA
                , set_pin                   = pnt               -- Calibration Datum Pin Reward Service Charges Withdrawl Authorization 
                }

    let tx   = Constraints.mustPayToTheScript datDatum $ Ada.lovelaceValueOf svcAmt
    ledgerTx <- submitTxConstraints calServiceRewardsInstance tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "Instrument Calibration Service Request " 
    logInfo @String $ printf "Instrument Equipment shall be calibrated with requirement Zero %d - Span %d - Percentages Accuracy %d - Percentages Repeatability %d " basezero basespan baseacc  baserep 
    logInfo @String $ printf "Calibration Service Charges of %d ADA will  be credited to Service Provider when the performance meet the specification" svcAmt

-- | The "Instrument Calibration Service Spesification Validation" contract endpoint.
calvalidationupdate :: AsContractError e => CalSpecValParams -> Contract () CalServiceRewardsSchema e ()
calvalidationupdate (CalSpecValParams valzerocal valspancal valacccal valrepcal pin_validation) = do
    onow   <- currentTime
    opkh   <- ownPaymentPubKeyHash
    -- filter all incorrect datum calibration service Rewards scripts
    unspentOutputs <- Map.filter hasCorrectDatum <$> utxosAt calServiceRewardsAddress
    let datRedeemer = CalServiceChargesRedeemer
                { val_ZeroCal    = valzerocal                 -- Validated Value of Instrument Zero Calibration
                , val_SpanCal    = valspancal                 -- Validated Value 0f Instrument Span Calibration Setting
                , val_AccCal     = valacccal			      -- Validated Value 0f Instrument Accuracy Calibration Setting
                , val_RepCal     = valrepcal		          -- Validated Value 0f Instrument Repeatability Calibration Setting                
                , val_pin        = pin_validation             -- Validated Reward Service Charges Withdrawl Authorization 
                }

    let tx = collectFromScript unspentOutputs datRedeemer
    ledgerTx <- submitTxConstraintsSpending calServiceRewardsInstance unspentOutputs tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "Instrument Calibration Validation Reports" 
    logInfo @String $ printf "Instrument has been calibrated with Zero %d - Span %d - Percentages Accuracy %d - Percentages Repeatability %d  " valzerocal valspancal valacccal valrepcal
      where
        hasCorrectDatum :: ChainIndexTxOut -> Bool
        hasCorrectDatum (ScriptChainIndexTxOut _ _ (Right (Datum datum)) _)    =
          case PlutusTx.fromBuiltinData datum of
          Just d  -> valzerocal == (set_ZeroCal d) && valspancal == (set_SpanCal d) && valacccal <= (set_Acc d) && valrepcal <= (set_Rep d) && pin_validation == (set_pin d)
          Nothing -> False
        hasCorrectDatum _ = False


-- | Instrument Calibration Services Rewards endpoints.
endpoints :: AsContractError e => Contract () CalServiceRewardsSchema e ()
endpoints = awaitPromise (calsvcrewards' `select` calvalidationupdate') >> endpoints
  where
    calsvcrewards'       = endpoint @"Instrument Calibration Service On Spesification Target and Service Charges Funds" calsvcrewards
    calvalidationupdate' = endpoint @"Instrument Calibration Service Validation" calvalidationupdate

mkSchemaDefinitions ''CalServiceRewardsSchema

