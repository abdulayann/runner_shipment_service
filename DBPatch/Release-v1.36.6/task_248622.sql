update consolidation_details
set receiving_branch=459
where id=24213;

Delete from network_transfer where id in (8056);