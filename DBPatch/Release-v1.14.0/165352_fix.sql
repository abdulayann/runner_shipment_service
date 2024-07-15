update shipment_details s set direction = 'IMP' , shipment_type ='IMP',updated_at =NOW() where s.tenant_id ='536' and shipment_id ='BOMS24060703';
