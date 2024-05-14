ALTER TABLE shipment_details
    ALTER COLUMN weight type decimal,
    ALTER COLUMN volume type decimal,
    ALTER COLUMN volumetric_weight type decimal,
    ALTER COLUMN chargable type decimal,
    ALTER COLUMN net_weight type decimal;