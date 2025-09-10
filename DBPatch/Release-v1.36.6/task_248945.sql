--consolidation no LNZS25084369
Delete from network_transfer where entity_id=69052 and tenant_id=459;

update consolidation_details
set receiving_branch=481
where id=69052;

--consolidation no LNZS25084366
Delete from network_transfer where entity_id=69035 and tenant_id=459;

update consolidation_details
set receiving_branch=470
where id=69035;


--consolidation no SGHCSS25082368
Delete from network_transfer where entity_id=63821 and tenant_id=481;