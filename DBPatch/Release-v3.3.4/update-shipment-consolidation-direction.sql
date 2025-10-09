Update consolidation_details
set shipment_type = 'IMP'
where id in (76068,81857,65656,78349,73746,65657);

Update shipment_details
set direction = 'IMP'
where id in (102138,86200,96140,86201,100060,99392,106549);