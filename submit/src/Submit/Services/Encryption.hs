module Submit.Services.Encryption 
    ( Encryption(..)
    ) where

import Submit.Models.Models

data Encryption f = Encryption
  { decrypt :: EncryptedSignKey -> f DecryptedSignKey
  }