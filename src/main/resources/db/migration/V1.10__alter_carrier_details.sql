Alter Table IF EXISTS carrier_details
    Add Column IF NOT EXISTS origin_port varchar,
    Add Column IF NOT EXISTS destination_port varchar;