module Template.View
  ( templateLibraryCard
  , contractSetupScreenHeader
  , contractSetupScreen
  , contractSetupConfirmationCard
  ) where

import Prelude hiding (div, min)
import Css (applyWhen, classNames, toggleWhen)
import Data.BigInteger (fromString) as BigInteger
import Data.Lens (view)
import Data.Map (Map, lookup)
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (toUnfoldable) as Set
import Data.Tuple.Nested ((/\))
import Halogen.HTML (HTML, a, button, div, div_, h2_, h3_, h4_, input, label, li, p_, span, text, ul, ul_)
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (InputType(..), disabled, for, id_, list, min, placeholder, readOnly, type_, value)
import MainFrame.Types (Action(..), Card(..), Screen(..))
import Marlowe.Extended (Contract, IntegerTemplateType(..), TemplateContent, _slotContent, _valueContent, getParties)
import Marlowe.Semantics (PubKey, Party(..))
import Template.Lenses (_contractNickname, _extendedContract, _metaData, _roleWallets, _template, _templateContent)
import Template.Types (ContractSetupScreen(..), MetaData, Template)
import Template.Types (Action(..), State) as Template
import Template.Validation (roleError, roleWalletsAreValid, slotError, valueError)
import WalletData.Types (WalletLibrary)
import WalletData.View (nicknamesDataList)

templateLibraryCard :: forall p. Array Template -> HTML p Action
templateLibraryCard templates =
  div_
    [ h2_ [ text "Start new from template" ]
    , div
        [ classNames [ "grid", "gap-1", "md:grid-cols-2", "lg:grid-cols-3" ] ]
        (templateBox <$> templates)
    ]

templateBox :: forall p. Template -> HTML p Action
templateBox template =
  div
    [ classNames [ "bg-white", "p-1" ] ]
    [ h4_
        [ text template.metaData.contractType ]
    , h3_
        [ text template.metaData.contractName ]
    , p_
        [ text template.metaData.contractDescription ]
    , button
        [ classNames [ "bg-green", "text-white" ]
        , onClick $ const $ Just $ SetTemplate template
        ]
        [ text "Setup" ]
    ]

contractSetupScreen :: forall p. WalletLibrary -> ContractSetupScreen -> Template.State -> HTML p Action
contractSetupScreen wallets setupScreen state =
  let
    contractNickname = view _contractNickname state

    metaData = view (_template <<< _metaData) state

    extendedContract = view (_template <<< _extendedContract) state

    roleWallets = view _roleWallets state

    templateContent = view _templateContent state
  in
    div
      []
      [ contractSetupScreenHeader setupScreen contractNickname
      , case setupScreen of
          ContractRolesScreen -> contractRolesScreen wallets metaData extendedContract roleWallets
          ContractParametersScreen -> contractParametersScreen metaData templateContent
          ContractReviewScreen -> contractReviewScreen state
      , div
          [ classNames [ "absolute", "bottom-1", "left-1", "right-1", "flex", "items-center", "justify-between" ] ]
          $ contractNavigationButtons setupScreen roleWallets wallets
      ]

contractSetupScreenHeader :: forall p. ContractSetupScreen -> String -> HTML p Action
contractSetupScreenHeader setupScreen contractNickname =
  div_
    [ div
        []
        [ input
            [ classNames [ "w-full", "text-center" ]
            , type_ InputText
            , placeholder "Contract name"
            , value contractNickname
            , onValueInput $ Just <<< TemplateAction <<< Template.SetContractNickname
            ]
        ]
    , div
        [ classNames [ "flex", "justify-between" ] ]
        [ span
            [ classNames $ screenClasses setupScreen ContractRolesScreen ]
            [ text "Roles" ]
        , span
            [ classNames $ screenClasses setupScreen ContractParametersScreen ]
            [ text "Parameters" ]
        , span
            [ classNames $ screenClasses setupScreen ContractReviewScreen ]
            [ text "Review" ]
        ]
    ]
  where
  screenClasses currentScreen screen = [ "p-1" ] <> applyWhen (currentScreen == screen) "text-green"

contractNavigationButtons :: forall p. ContractSetupScreen -> Map String String -> WalletLibrary -> Array (HTML p Action)
contractNavigationButtons screen roleWallets wallets = case screen of
  ContractRolesScreen ->
    [ a
        [ onClick $ const $ Just $ ToggleCard TemplateLibraryCard ]
        [ text "< Library quick access" ]
    , button
        [ onClick $ const $ Just $ SetScreen $ ContractSetupScreen ContractParametersScreen
        , disabled $ not $ roleWalletsAreValid roleWallets wallets
        ]
        [ text "Next >" ]
    ]
  ContractParametersScreen ->
    [ a
        [ onClick $ const $ Just $ SetScreen $ ContractSetupScreen ContractRolesScreen ]
        [ text "< Roles" ]
    , button
        [ onClick $ const $ Just $ SetScreen $ ContractSetupScreen ContractReviewScreen ]
        [ text "Next >" ]
    ]
  ContractReviewScreen ->
    [ a
        [ onClick $ const $ Just $ SetScreen $ ContractSetupScreen ContractParametersScreen ]
        [ text "< Parameters" ]
    , button
        [ onClick $ const $ Just $ ToggleCard ContractSetupConfirmationCard ]
        [ text "Pay and start >" ]
    ]

contractRolesScreen :: forall p. WalletLibrary -> MetaData -> Contract -> Map String PubKey -> HTML p Action
contractRolesScreen wallets metaData extendedContract roleWallets =
  ul
    [ classNames [ "mx-auto", "w-card" ] ]
    $ map partyInput (Set.toUnfoldable $ getParties extendedContract)
  where
  partyInput (PK pubKey) =
    li
      [ classNames [ "mb-1" ] ]
      [ label
          [ classNames [ "block", "mb-0.5" ]
          , for pubKey
          ]
          [ text "Wallet" ]
      , input
          [ classNames [ "w-full" ]
          , id_ pubKey
          , type_ InputText
          , value pubKey
          , readOnly true
          ]
      ]

  partyInput (Role tokenName) =
    let
      description = fromMaybe "no description available" $ lookup tokenName metaData.roleDescriptions

      assigned = fromMaybe "" $ lookup tokenName roleWallets

      mRoleError = roleError assigned wallets
    in
      li
        [ classNames [ "mb-1" ] ]
        [ label
            [ classNames [ "block", "mb-0.5" ]
            , for tokenName
            ]
            [ text $ tokenName <> ": " <> description ]
        , input
            [ classNames $ [ "w-full" ] <> toggleWhen (mRoleError == Nothing) "border-green" "border-red"
            , id_ tokenName
            , type_ InputText
            , list "walletNicknames"
            , onValueInput $ Just <<< TemplateAction <<< Template.SetRoleWallet tokenName
            , value assigned
            ]
        , div
            [ classNames [ "mb-1", "text-red", "text-sm" ] ]
            $ case mRoleError of
                Just roleError -> [ text $ show roleError ]
                Nothing -> []
        , nicknamesDataList wallets
        ]

contractParametersScreen :: forall p. MetaData -> TemplateContent -> HTML p Action
contractParametersScreen metaData templateContent =
  let
    slotContent = view _slotContent templateContent

    valueContent = view _valueContent templateContent
  in
    div
      [ classNames [ "mx-auto", "w-card" ] ]
      [ ul
          [ classNames [ "mb-1" ] ]
          $ map (parameterInput SlotContent) (Map.toUnfoldable slotContent)
      , ul_
          $ map (parameterInput ValueContent) (Map.toUnfoldable valueContent)
      ]
  where
  parameterInput integerTemplateType (key /\ parameterValue) =
    let
      description = case integerTemplateType of
        SlotContent -> fromMaybe "no description available" $ lookup key metaData.slotParameterDescriptions
        ValueContent -> fromMaybe "no description available" $ lookup key metaData.valueParameterDescriptions

      mParameterError = case integerTemplateType of
        SlotContent -> slotError parameterValue
        ValueContent -> valueError parameterValue
    in
      li
        [ classNames [ "mb-1" ] ]
        [ label
            [ classNames [ "block", "mb-0.5" ] ]
            [ text $ key <> ": " <> description ]
        , input
            [ classNames $ [ "w-full" ] <> toggleWhen (mParameterError == Nothing) "border-green" "border-red"
            , type_ InputNumber
            , min one
            , onValueInput $ Just <<< TemplateAction <<< Template.SetParameter integerTemplateType key <<< BigInteger.fromString
            , value $ show parameterValue
            ]
        , div
            [ classNames [ "mb-1", "text-red", "text-sm" ] ]
            $ case mParameterError of
                Just parameterError -> [ text $ show parameterError ]
                Nothing -> []
        ]

contractReviewScreen :: forall p. Template.State -> HTML p Action
contractReviewScreen state =
  div
    [ classNames [ "mx-auto", "w-card", "bg-white", "p-1" ] ]
    [ text "Summary information about the contract goes here." ]

contractSetupConfirmationCard :: forall p. HTML p Action
contractSetupConfirmationCard =
  div_
    [ p_ [ text "Are you sure?" ]
    , div
        [ classNames [ "flex" ] ]
        [ button
            [ classNames [ "flex-1", "mr-1" ]
            , onClick $ const $ Just StartContract
            ]
            [ text "Pay and start" ]
        , button
            [ classNames [ "flex-1" ]
            , onClick $ const $ Just $ ToggleCard ContractSetupConfirmationCard
            ]
            [ text "Cancel" ]
        ]
    ]
