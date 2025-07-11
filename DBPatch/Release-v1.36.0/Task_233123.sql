UPDATE network_transfer
Set status = 'ACCEPTED'
where id in (1866, 1870);

UPDATE consolidation_details
Set is_transferred_to_receiving_branch = true
where id in (41334, 41335);