ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS isf_file_main_page VARCHAR;