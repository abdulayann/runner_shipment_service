package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.section.request.SectionVisibilityRequest;
import com.dpw.runner.shipment.services.dto.section.response.SectionVisibilityResponse;
import java.util.List;

public interface ISectionVisibilityService {

  IRunnerResponse create(SectionVisibilityRequest sectionVisibilityRequest);

  IRunnerResponse update(SectionVisibilityRequest sectionVisibilityRequest);

  IRunnerResponse getAll();

  IRunnerResponse getById(Long id);

  IRunnerResponse delete(Long id);

  IRunnerResponse findByBranchModeDirection(String branch, String mode, String direction);

  IRunnerResponse getAllPaginated(int page, int size, String sortBy, String sortDirection);

  List<SectionVisibilityResponse> getByBranchCode(String code);
}
