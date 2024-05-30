ALTER TABLE IF EXISTS packing
    ALTER COLUMN weight TYPE DECIMAL,
    ALTER COLUMN volume TYPE DECIMAL,
    ALTER COLUMN length TYPE DECIMAL,
    ALTER COLUMN width TYPE DECIMAL,
    ALTER COLUMN height TYPE DECIMAL,
    ALTER COLUMN min_temp TYPE DECIMAL,
    ALTER COLUMN max_temp TYPE DECIMAL,
    ALTER COLUMN net_weight TYPE DECIMAL,
    ALTER COLUMN volume_weight TYPE DECIMAL,
    ALTER COLUMN chargeable TYPE DECIMAL;

ALTER TABLE IF EXISTS containers
    ALTER COLUMN net_weight TYPE DECIMAL,
    ALTER COLUMN gross_weight TYPE DECIMAL,
    ALTER COLUMN measurement TYPE DECIMAL,
    ALTER COLUMN gross_volume TYPE DECIMAL,
    ALTER COLUMN min_temp TYPE DECIMAL,
    ALTER COLUMN max_temp TYPE DECIMAL,
    ALTER COLUMN tare_weight TYPE DECIMAL,
    ALTER COLUMN package_length TYPE DECIMAL,
    ALTER COLUMN package_breadth TYPE DECIMAL,
    ALTER COLUMN package_height TYPE DECIMAL,
    ALTER COLUMN chargeable TYPE DECIMAL,
    ALTER COLUMN allocated_weight TYPE DECIMAL,
    ALTER COLUMN allocated_volume TYPE DECIMAL,
    ALTER COLUMN achieved_weight TYPE DECIMAL,
    ALTER COLUMN achieved_volume TYPE DECIMAL;


ALTER TABLE IF EXISTS shipment_additional_details
    ALTER COLUMN shipper_cod TYPE DECIMAL,
    ALTER COLUMN spot_rate TYPE DECIMAL,
    ALTER COLUMN free_days TYPE DECIMAL,
    ALTER COLUMN invoice_value TYPE DECIMAL,
    ALTER COLUMN assess_value TYPE DECIMAL,
    ALTER COLUMN cif_value TYPE DECIMAL,
    ALTER COLUMN total_duty TYPE DECIMAL;

ALTER TABLE IF EXISTS achieved_quantities
    ALTER COLUMN weight_volume TYPE DECIMAL,
    ALTER COLUMN consolidated_weight TYPE DECIMAL,
    ALTER COLUMN consolidated_volume TYPE DECIMAL,
    ALTER COLUMN consolidation_charge_quantity TYPE DECIMAL;

ALTER TABLE IF EXISTS allocations
    ALTER COLUMN weight TYPE DECIMAL,
    ALTER COLUMN volume TYPE DECIMAL,
    ALTER COLUMN chargeable TYPE DECIMAL,
    ALTER COLUMN min_temp TYPE DECIMAL,
    ALTER COLUMN max_temp TYPE DECIMAL;

ALTER TABLE IF EXISTS pickup_delivery_details
    ALTER COLUMN labour_charge TYPE DECIMAL,
    ALTER COLUMN truck_wait_time_charge TYPE DECIMAL,
    ALTER COLUMN storage_charge TYPE DECIMAL;