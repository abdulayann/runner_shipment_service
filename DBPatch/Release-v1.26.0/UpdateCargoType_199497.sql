Update customer_booking set cargo_type = 'LCL', updated_at = NOW() where id in ('23041', '23047');
Update shipment_details set shipment_type = 'LCL', updated_at = NOW() where id in ('31082', '31074');