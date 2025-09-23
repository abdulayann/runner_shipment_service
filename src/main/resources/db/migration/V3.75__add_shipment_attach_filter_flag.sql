ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS include_ports_in_ship_console_attach_filter BOOLEAN DEFAULT true;