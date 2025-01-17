package com.dpw.runner.shipment.services.repository.interfaces;


import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.response.Consolidation.ConsolidationLiteResponse;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

public interface ICustomConsolidationDetailsRepository {
  Page<ConsolidationLiteResponse> findAllLiteConsol(Specification<ConsolidationDetails> spec, Pageable pageable);
}
