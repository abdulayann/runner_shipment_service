DELETE FROM triangulation_partner_consolidation WHERE consolidation_id in(select id from consolidation_details where consolidation_number in('SNZCSS25060551') and
  tenant_id = 713);