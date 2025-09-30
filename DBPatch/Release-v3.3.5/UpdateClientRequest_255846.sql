update bill_charge_revenue_details set
    measurement_basis_unit = 'Containers',
    measurement_basis_quantity = 1,
    measurement_basis = 'Container_Count'
    unit_rate = 150,
    unit_rate_currency = 'USD',
    updated_at = NOW(),
    where sequence_id ='BCR695502';