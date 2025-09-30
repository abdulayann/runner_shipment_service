ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS print_after_each_page_sea_way_bill boolean,
    ADD COLUMN IF NOT EXISTS sea_way_bill_back_page varchar(255);
