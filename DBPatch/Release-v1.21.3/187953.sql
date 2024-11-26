update shipment_details
set sales_agent = null,
updated_at = GETDATE()
where id = 20291;