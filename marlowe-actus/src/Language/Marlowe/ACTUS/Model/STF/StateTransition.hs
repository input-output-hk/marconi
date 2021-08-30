{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.STF.StateTransition where

import           Data.Maybe                                             (fromJust, fromMaybe, maybeToList)
import           Data.Time                                              (Day)
import           Language.Marlowe.ACTUS.Definitions.BusinessEvents      (EventType (..), RiskFactors (..))
import           Language.Marlowe.ACTUS.Definitions.ContractState       (ContractState, ContractStatePoly (..))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms       (CT (..), ContractTerms (..), ScheduleConfig)
import           Language.Marlowe.ACTUS.Definitions.Schedule            (ShiftedDay (calculationDay))
import           Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule    (schedule)
import           Language.Marlowe.ACTUS.Model.STF.StateTransitionModel
import           Language.Marlowe.ACTUS.Model.Utility.ANN.Maturity      (maturity)
import           Language.Marlowe.ACTUS.Model.Utility.DateShift
import           Language.Marlowe.ACTUS.Model.Utility.ScheduleGenerator (inf, sup)
import           Language.Marlowe.ACTUS.Ops                             (YearFractionOps (_y))


shift :: ScheduleConfig -> Day -> ShiftedDay
shift = applyBDCWithCfg

stateTransition :: EventType -> RiskFactors -> ContractTerms -> ContractState -> Day -> ContractState
stateTransition ev RiskFactors{..} terms@ContractTerms{..} st@ContractStatePoly{..} t =
    let
        ct_DCC'   = fromJust ct_DCC
        ct_FER'   = fromJust ct_FER
        ct_RRLF'  = fromJust ct_RRLF
        ct_RRLC'  = fromJust ct_RRLC
        ct_RRPC'  = fromJust ct_RRPC
        ct_RRPF'  = fromJust ct_RRPF
        ct_RRMLT' = fromJust ct_RRMLT
        ct_RRSP'  = fromJust ct_RRSP
        ct_SCEF'  = fromJust ct_SCEF
        ct_SCIED' = fromJust ct_SCIED -- TODO: check for other CTs
        ct_SCCDD' = fromJust ct_SCCDD

        fpSchedule         = schedule FP terms
        prSchedule         = schedule PR terms

        tfp_minus          = maybe t calculationDay ((\sc -> sup sc t) =<< fpSchedule)
        tfp_plus           = maybe t calculationDay ((\sc -> inf sc t) =<< fpSchedule)

        tpr_minus          = maybe t calculationDay ((\sc -> sup sc t) =<< prSchedule)
        tpr_plus           = maybe t calculationDay ((\sc -> inf sc t) =<< prSchedule)

        y_sd_t             = _y ct_DCC' sd t ct_MD
        y_tfpminus_t       = _y ct_DCC' tfp_minus t ct_MD
        y_tfpminus_tfpplus = _y ct_DCC' tfp_minus tfp_plus ct_MD
        y_ipanx_t          = _y ct_DCC' (fromJust ct_IPANX) t ct_MD

        y_tprminus_t       = _y ct_DCC' tpr_minus t ct_MD
        y_tprminus_tprplus = _y ct_DCC' tpr_minus tpr_plus ct_MD

        y_t                = _y ct_DCC' t tpr_plus ct_MD
    in case contractType of
        PAM ->
            case ev of
                AD   -> _STF_AD_PAM st t y_sd_t
                IED  -> _STF_IED_PAM st t y_ipanx_t ct_IPNR ct_IPANX ct_CNTRL ct_IPAC (fromJust ct_NT)
                MD   -> _STF_MD_PAM st t
                PP   -> _STF_PP_PAM st t pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                PY   -> _STF_PY_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                FP   -> _STF_FP_PAM st t y_sd_t
                PRD  -> _STF_PRD_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                TD   -> _STF_TD_PAM st t
                IP   -> _STF_IP_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                IPCI -> _STF_IPCI_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                RR   -> _STF_RR_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_RRLF' ct_RRLC' ct_RRPC' ct_RRPF' ct_RRMLT' ct_RRSP' o_rf_RRMO
                RRF  -> _STF_RRF_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_RRNXT
                SC   -> _STF_SC_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_SCEF' o_rf_SCMO ct_SCIED'
                CE   -> _STF_CE_PAM st t y_sd_t
                _    -> st
        LAM ->
            case ev of
                AD   -> _STF_AD_PAM st t y_sd_t
                IED  -> _STF_IED_LAM st t y_ipanx_t ct_IPNR ct_IPANX ct_CNTRL ct_IPAC (fromJust ct_NT) ct_IPCB ct_IPCBA
                PR   -> _STF_PR_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_IPCB
                MD   -> _STF_MD_LAM st t
                PP   -> _STF_PP_LAM st t pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_IPCB
                PY   -> _STF_PY_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                FP   -> _STF_FP_LAM st t y_sd_t
                PRD  -> _STF_PRD_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                TD   -> _STF_TD_PAM st t
                IP   -> _STF_IP_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                IPCI -> _STF_IPCI_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_IPCB
                IPCB -> _STF_IPCB_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                RR   -> _STF_RR_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_RRLF' ct_RRLC' ct_RRPC' ct_RRPF' ct_RRMLT' ct_RRSP' o_rf_RRMO
                RRF  -> _STF_RRF_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_RRNXT
                SC   -> _STF_SC_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_SCEF' o_rf_SCMO ct_SCCDD'
                CE   -> _STF_CE_PAM st t y_sd_t
                _    -> st
        NAM ->
            case ev of
                AD   -> _STF_AD_PAM st t y_sd_t
                IED  -> _STF_IED_LAM st t y_ipanx_t ct_IPNR ct_IPANX ct_CNTRL ct_IPAC (fromJust ct_NT) ct_IPCB ct_IPCBA
                PR   -> _STF_PR_NAM st t pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_IPCB
                MD   -> _STF_MD_LAM st t
                PP   -> _STF_PP_LAM st t pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_IPCB
                PY   -> _STF_PY_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                FP   -> _STF_FP_LAM st t y_sd_t
                PRD  -> _STF_PRD_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                TD   -> _STF_TD_PAM st t
                IP   -> _STF_IP_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                IPCI -> _STF_IPCI_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_IPCB
                IPCB -> _STF_IPCB_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                RR   -> _STF_RR_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_RRLF' ct_RRLC' ct_RRPC' ct_RRPF' ct_RRMLT' ct_RRSP' o_rf_RRMO
                RRF  -> _STF_RRF_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_RRNXT
                SC   -> _STF_SC_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_SCEF' o_rf_SCMO ct_SCIED'
                CE   -> _STF_AD_PAM st t y_sd_t
                _    -> st

        ANN ->
          let prDates            = maybe [] (map calculationDay) prSchedule ++ maybeToList (maturity terms)
              prDatesAfterSd     = filter (\d -> d > sd) prDates
              ti                 = zipWith (\tn tm -> _y (fromJust ct_DCC) tn tm ct_MD) prDatesAfterSd (tail prDatesAfterSd)
          in
            case ev of
                AD   -> _STF_AD_PAM st t y_sd_t
                IED  -> _STF_IED_LAM st t y_ipanx_t ct_IPNR ct_IPANX ct_CNTRL ct_IPAC (fromJust ct_NT) ct_IPCB ct_IPCBA
                PR   -> _STF_PR_NAM st t pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_IPCB
                MD   -> _STF_MD_LAM st t
                PP   -> _STF_PP_LAM st t pp_payoff y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_IPCB
                PY   -> _STF_PY_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                FP   -> _STF_FP_LAM st t y_sd_t
                PRD  -> _STF_PRD_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                TD   -> _STF_TD_PAM st t
                IP   -> _STF_IP_PAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                IPCI -> _STF_IPCI_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_IPCB
                IPCB -> _STF_IPCB_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL
                RR   -> _STF_RR_ANN st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_RRLF' ct_RRLC' ct_RRPC' ct_RRPF' ct_RRMLT' ct_RRSP' o_rf_RRMO ti
                RRF  -> _STF_RRF_ANN st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL (fromJust ct_RRNXT) ti
                SC   -> _STF_SC_LAM st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus ct_FEB ct_FER' ct_CNTRL ct_SCEF' o_rf_SCMO ct_SCIED'
                CE   -> _STF_AD_PAM st t y_sd_t
                PRF  -> _STF_PRF_ANN st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus y_tprminus_t y_tprminus_tprplus ct_FEB ct_FER' ct_CNTRL ct_RRNXT y_t ti
                _    -> st


