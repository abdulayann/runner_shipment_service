ALTER TABLE truck_driver_details
ADD COLUMN if NOT EXISTS third_party_transporter bigint
REFERENCES parties(id);