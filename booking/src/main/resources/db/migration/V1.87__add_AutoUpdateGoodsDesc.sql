ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS auto_update_goods_desc BOOLEAN;