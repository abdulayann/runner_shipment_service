package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.section.request.SectionFieldsRequest;

public interface ISectionFieldsService {

  IRunnerResponse create(SectionFieldsRequest sectionFieldsRequest);

  IRunnerResponse update(SectionFieldsRequest sectionFieldsRequest);

  IRunnerResponse getAll();

  IRunnerResponse getById(Long id);

  IRunnerResponse delete(Long id);
}
