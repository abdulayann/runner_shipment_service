ALTER TABLE shipment_details
    ADD COLUMN IF NOT EXISTS carrier_doc_cut_off TIMESTAMP,
    ADD COLUMN IF NOT EXISTS cargo_receipt_w_h_cut_off TIMESTAMP,
    ADD COLUMN IF NOT EXISTS last_free_date_cut_off TIMESTAMP,
    ADD COLUMN IF NOT EXISTS number_of_free_days_cut_off INTEGER;


ALTER TABLE consolidation_details
    ADD COLUMN IF NOT EXISTS carrier_doc_cut_off TIMESTAMP,
    ADD COLUMN IF NOT EXISTS cargo_receipt_w_h_cut_off TIMESTAMP,
    ADD COLUMN IF NOT EXISTS last_free_date_cut_off TIMESTAMP,
    ADD COLUMN IF NOT EXISTS number_of_free_days_cut_off INTEGER;

ALTER TABLE customer_booking
    ADD COLUMN IF NOT EXISTS carrier_doc_cut_off TIMESTAMP,
    ADD COLUMN IF NOT EXISTS cargo_receipt_w_h_cut_off TIMESTAMP,
    ADD COLUMN IF NOT EXISTS last_free_date_cut_off TIMESTAMP,
    ADD COLUMN IF NOT EXISTS number_of_free_days_cut_off INTEGER;