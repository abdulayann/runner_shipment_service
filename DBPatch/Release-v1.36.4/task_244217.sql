update consolidation_details
set receiving_branch=474
where id=51715;

Delete from network_transfer where id in (4868, 3644);