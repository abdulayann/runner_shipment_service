package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.section.request.SectionVisibilityRequest;

public interface ISectionVisibilityService {

  IRunnerResponse create(SectionVisibilityRequest sectionVisibilityRequest);

  IRunnerResponse update(SectionVisibilityRequest sectionVisibilityRequest);

  IRunnerResponse getAll();

  IRunnerResponse getById(Long id);

  IRunnerResponse delete(Long id);
  IRunnerResponse findByTenantIdModeDirection(Integer tenantId, String mode, String direction);

}
