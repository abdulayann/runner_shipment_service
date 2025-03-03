package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.section.request.SectionDetailsRequest;

public interface ISectionDetailsService {

  IRunnerResponse create(SectionDetailsRequest sectionDetailsRequest);

  IRunnerResponse update(SectionDetailsRequest sectionDetailsRequest);

  IRunnerResponse getAll();

  IRunnerResponse getById(Long id);

  IRunnerResponse delete(Long id);
}
