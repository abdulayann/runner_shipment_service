Alter Table IF EXISTS shipment_details
    Add Column IF NOT EXISTS job_status varchar(10);