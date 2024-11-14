update shipment_details
set sales_agent = null
where id = 20291 and updated_at = GETDATE();