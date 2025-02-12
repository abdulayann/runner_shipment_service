ALTER TABLE IF EXISTS parties
  ADD COLUMN IF NOT EXISTS country_code varchar (32);