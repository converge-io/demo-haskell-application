-- | An `isbn` package exists that contains all of this but we're adding it ourselves
-- for the sake of having a realistic example of smart constructors
module Books.Domain.ISBN
-- Since we're not exposing the constructors
-- this makes it so consumers of our module are only able to create ISBN
-- values through the `mkISBN` function. This means that (assuming our validations are correct)
-- it's essentially impossible to encounter invalid ISBN values as long as we use the ISBN type
  ( ISBN,
    mkISBN,
    renderISBN,
  )
where

import qualified RIO.Char as Char
import qualified RIO.Char.Partial as Partial
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as Partial

data ISBN
  = -- | An ISBN-10 value. Consists of 9 digits followed by a base-11 check digit (@0-9@ or @\'X\'@).
    ISBN10 Text
  | -- | An ISBN-13 value. Consists of 12 digits followed by a base-10 check digit (@0-9@).
    ISBN13 Text
  deriving (Show, Eq)

mkISBN :: Text -> Either Text ISBN
mkISBN = mapLeft explainError . validateISBN

renderISBN :: ISBN -> Text
renderISBN (ISBN10 i) = i
renderISBN (ISBN13 i) = i

validateISBN :: Text -> Either ISBNValidationError ISBN
validateISBN isbn = do
  let isbn10result = validateISBN10 isbn
      isbn13result = validateISBN13 isbn

  case (isbn10result, isbn13result) of
    (Right isbn10, _) ->
      Right isbn10
    (_, Right isbn13) ->
      Right isbn13
    (Left ISBN10InvalidInputLength, Left ISBN13InvalidInputLength) ->
      Left InvalidISBNInputLength
    (Left ISBN10IllegalCharactersInBody, _) ->
      Left IllegalCharactersInISBN10Body
    (Left ISBN10IllegalCharacterAsCheckDigit, _) ->
      Left IllegalCharacterAsISBN10CheckDigit
    (_, Left ISBN13IllegalCharactersInInput) ->
      Left IllegalCharactersInISBN13Input
    (Left ISBN10InvalidCheckDigit, _) ->
      Left InvalidISBN10CheckDigit
    (_, Left ISBN13InvalidCheckDigit) ->
      Left InvalidISBN13CheckDigit

-- | Possible validation errors resulting from ISBN validation. Can be
-- rendered as a descriptive error message using 'renderISBNValidationError'.
data ISBNValidationError
  = -- | The length of the input string is not 10 or 13 characters, not counting hyphens
    InvalidISBNInputLength
  | -- | The first nine characters of the ISBN-10 input contain non-numeric characters
    IllegalCharactersInISBN10Body
  | -- | The ISBN-13 input contains non-numeric characters
    IllegalCharactersInISBN13Input
  | -- | The check digit of the ISBN-10 is not a valid character (@0-9@ or @\'X\'@)
    IllegalCharacterAsISBN10CheckDigit
  | -- | The check digit is not valid for the given ISBN-10
    InvalidISBN10CheckDigit
  | -- | The check digit is not valid for the given ISBN-13
    InvalidISBN13CheckDigit
  deriving (Show, Eq)

validateISBN10 :: Text -> Either ISBN10ValidationError ISBN
validateISBN10 input = do
  -- Make a copy of the text input before further manipulation to prevent
  -- space leaks if input text is a slice of a larger string
  let inputWithoutHyphens = Text.filter (/= '-') $ Text.copy input

  unless (Text.length inputWithoutHyphens == 10) $
    Left ISBN10InvalidInputLength

  let invalidBodyCharacters = Text.filter (not . Char.isNumber) (Partial.init inputWithoutHyphens)

  unless (Text.length invalidBodyCharacters == 0) $
    Left ISBN10IllegalCharactersInBody

  unless (isValidISBN10CheckDigit $ Partial.last inputWithoutHyphens) $
    Left ISBN10IllegalCharacterAsCheckDigit

  unless (confirmISBN10CheckDigit inputWithoutHyphens) $
    Left ISBN10InvalidCheckDigit

  pure $ ISBN10 inputWithoutHyphens
  where
    confirmISBN10CheckDigit :: Text -> Bool
    confirmISBN10CheckDigit isbn10 =
      calculateISBN10CheckDigitValue (Partial.init isbn10) == isbn10CharToNumericValue (Partial.last isbn10)

    calculateISBN10CheckDigitValue :: Text -> Int
    calculateISBN10CheckDigitValue text =
      go 10 (Text.unpack text) 0
      where
        go n charList acc =
          case charList of
            [] -> (11 - (acc `mod` 11)) `mod` 11
            c : clist -> go (n - 1) clist (acc + isbn10CharToNumericValue c * n)

    isbn10CharToNumericValue :: Char -> Int
    isbn10CharToNumericValue 'X' = 10
    isbn10CharToNumericValue c = Partial.digitToInt c

    isValidISBN10CheckDigit :: Char -> Bool
    isValidISBN10CheckDigit char = char `elem` ("1234567890X" :: String)

-- | Possible validation errors resulting from ISBN-10 validation.
data ISBN10ValidationError
  = -- | The length of the input string is not 10 characters, not counting hyphens
    ISBN10InvalidInputLength
  | -- | The first nine characters of the ISBN-10 input contain non-numeric characters
    ISBN10IllegalCharactersInBody
  | -- | The check digit of the ISBN-10 is not a valid character (@0-9@ or @\'X\'@)
    ISBN10IllegalCharacterAsCheckDigit
  | -- | The check digit is not valid for the given ISBN-10
    ISBN10InvalidCheckDigit
  deriving (Show, Eq)

validateISBN13 :: Text -> Either ISBN13ValidationError ISBN
validateISBN13 input = do
  -- Make a copy of the text input before further manipulation to prevent
  -- space leaks if input text is a slice of a larger string
  let inputWithoutHyphens = Text.filter (/= '-') $ Text.copy input

  unless (Text.length inputWithoutHyphens == 13) $
    Left ISBN13InvalidInputLength

  let illegalCharacters = Text.filter (not . Char.isNumber) inputWithoutHyphens

  unless (Text.length illegalCharacters == 0) $
    Left ISBN13IllegalCharactersInInput

  unless (confirmISBN13CheckDigit inputWithoutHyphens) $
    Left ISBN13InvalidCheckDigit

  pure $ ISBN13 inputWithoutHyphens
  where
    confirmISBN13CheckDigit :: Text -> Bool
    confirmISBN13CheckDigit isbn13 =
      calculateISBN13CheckDigitValue (Partial.init isbn13) == isbn13CharToNumericValue (Partial.last isbn13)

    calculateISBN13CheckDigitValue :: Text -> Int
    calculateISBN13CheckDigitValue text =
      go 1 (Text.unpack text) 0
      where
        go w charList acc =
          case charList of
            [] -> (10 - (acc `mod` 10)) `mod` 10
            c : clist -> go ((w + 2) `mod` 4) clist (acc + w * isbn13CharToNumericValue c)

    isbn13CharToNumericValue :: Char -> Int
    isbn13CharToNumericValue = Partial.digitToInt

-- | Possible validation errors resulting from ISBN-13 validation.
data ISBN13ValidationError
  = -- | The length of the input string is not 13 characters, not counting hyphens
    ISBN13InvalidInputLength
  | -- | The ISBN-13 input contains non-numeric characters
    ISBN13IllegalCharactersInInput
  | -- | The check digit is not valid for the given ISBN-13
    ISBN13InvalidCheckDigit
  deriving (Show, Eq)

explainError :: ISBNValidationError -> Text
explainError validationError =
  case validationError of
    InvalidISBNInputLength ->
      "An ISBN must be 10 or 13 characters, not counting hyphens"
    IllegalCharactersInISBN10Body ->
      "The first nine non-hyphen characters of an ISBN-10 must all be numbers"
    IllegalCharactersInISBN13Input ->
      "Every non-hyphen character of an ISBN-13 must be a number"
    IllegalCharacterAsISBN10CheckDigit ->
      "The last character of an ISBN-10 must be a number or the letter 'X'"
    InvalidISBN10CheckDigit ->
      "The supplied ISBN-10 is not valid"
    InvalidISBN13CheckDigit ->
      "The supplied ISBN-13 is not valid"
