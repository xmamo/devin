module Unicode (
  Category (..),
  category,
  isSpace,
  isNewline
) where

import System.IO.Unsafe

import qualified GI.GLib as GLib


data Category where
  UppercaseLetter :: Category
  LowercaseLetter :: Category
  TitlecaseLetter :: Category
  ModifierLetter :: Category
  OtherLetter :: Category
  NonspacingMark :: Category
  SpacingMark :: Category
  EnclosingMark :: Category
  DecimalNumber :: Category
  LetterNumber :: Category
  OtherNumber :: Category
  ConnectorPunctuation :: Category
  DashPunctuation :: Category
  OpenPunctuation :: Category
  ClosePunctuation :: Category
  InitialPunctuation :: Category
  FinalPunctuation :: Category
  OtherPunctuation :: Category
  MathSymbol :: Category
  CurrencySymbol :: Category
  ModifierSymbol :: Category
  OtherSymbol :: Category
  SpaceSeparator :: Category
  LineSeparator :: Category
  ParagraphSeparator :: Category
  Control :: Category
  Format :: Category
  Surrogate :: Category
  PrivateUse :: Category
  Unassigned :: Category
  Other :: Category
  deriving (Eq, Show, Read)


category :: Char -> Category
category c = case unsafePerformIO (GLib.unicharType c) of
  GLib.UnicodeTypeUppercaseLetter -> UppercaseLetter
  GLib.UnicodeTypeLowercaseLetter -> LowercaseLetter
  GLib.UnicodeTypeTitlecaseLetter -> TitlecaseLetter
  GLib.UnicodeTypeModifierLetter -> ModifierLetter
  GLib.UnicodeTypeOtherLetter -> OtherLetter
  GLib.UnicodeTypeNonSpacingMark -> NonspacingMark
  GLib.UnicodeTypeSpacingMark -> SpacingMark
  GLib.UnicodeTypeEnclosingMark -> EnclosingMark
  GLib.UnicodeTypeDecimalNumber -> DecimalNumber
  GLib.UnicodeTypeLetterNumber -> LetterNumber
  GLib.UnicodeTypeOtherNumber -> OtherNumber
  GLib.UnicodeTypeConnectPunctuation -> ConnectorPunctuation
  GLib.UnicodeTypeDashPunctuation -> DashPunctuation
  GLib.UnicodeTypeOpenPunctuation -> OpenPunctuation
  GLib.UnicodeTypeClosePunctuation -> ClosePunctuation
  GLib.UnicodeTypeInitialPunctuation -> InitialPunctuation
  GLib.UnicodeTypeFinalPunctuation -> FinalPunctuation
  GLib.UnicodeTypeOtherPunctuation -> OtherPunctuation
  GLib.UnicodeTypeMathSymbol -> MathSymbol
  GLib.UnicodeTypeCurrencySymbol -> CurrencySymbol
  GLib.UnicodeTypeModifierSymbol -> ModifierSymbol
  GLib.UnicodeTypeOtherSymbol -> OtherSymbol
  GLib.UnicodeTypeSpaceSeparator -> SpaceSeparator
  GLib.UnicodeTypeLineSeparator -> LineSeparator
  GLib.UnicodeTypeParagraphSeparator -> ParagraphSeparator
  GLib.UnicodeTypeControl -> Control
  GLib.UnicodeTypeFormat -> Format
  GLib.UnicodeTypeSurrogate -> Surrogate
  GLib.UnicodeTypePrivateUse -> PrivateUse
  GLib.UnicodeTypeUnassigned -> Unassigned
  GLib.AnotherUnicodeType _ -> Other


isSpace :: Char -> Bool
isSpace = unsafePerformIO . GLib.unicharIsspace


isNewline :: Char -> Bool
isNewline '\n' = True
isNewline '\v' = True
isNewline '\r' = True
isNewline '\x85' = True
isNewline '\x2028' = True
isNewline '\x2029' = True
isNewline _ = False
