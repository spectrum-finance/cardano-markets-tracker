data Encryption f = Encryption
  { decrypt :: EncryptedSignKey -> f DecryptedSignKey
  }