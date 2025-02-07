package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.section.request.SectionVisibilityRequest;
import com.dpw.runner.shipment.services.dto.section.response.SectionDetailsResponse;
import com.dpw.runner.shipment.services.dto.section.response.SectionFieldsResponse;
import com.dpw.runner.shipment.services.dto.section.response.SectionVisibilityResponse;
import com.dpw.runner.shipment.services.entity.SectionDetails;
import com.dpw.runner.shipment.services.entity.SectionVisibility;
import com.dpw.runner.shipment.services.exception.exceptions.SectionVisibilityException;
import com.dpw.runner.shipment.services.repository.interfaces.ISectionDetailsRepository;
import com.dpw.runner.shipment.services.repository.interfaces.ISectionVisibilityRepository;
import com.dpw.runner.shipment.services.service.interfaces.ISectionVisibilityService;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

@Service
public class SectionVisibilityServiceImpl implements ISectionVisibilityService {

  @Autowired
  private ISectionVisibilityRepository repository;

  @Autowired
  private ISectionDetailsRepository sectionDetailsRepository;

  @Autowired
  private ModelMapper modelMapper;

  @Override
  public IRunnerResponse create(SectionVisibilityRequest request) {
    // Check for unique constraint violation
    if (repository.existsByBranchAndModeAndDirection(request.getBranch(), request.getMode(),
        request.getDirection())) {
      throw new SectionVisibilityException(
          "SectionVisibility with the same tenantId, mode, and direction already exists.");
    }

    SectionVisibility sectionVisibility = modelMapper.map(request, SectionVisibility.class);

    // Validate and Map SectionDetails
    if (request.getSectionDetailIds() != null && !request.getSectionDetailIds().isEmpty()) {
      Set<SectionDetails> sectionDetails = validateAndGetSectionDetails(
          request.getSectionDetailIds());
      sectionVisibility.setSections(sectionDetails);
    }

    return modelMapper.map(repository.save(sectionVisibility), SectionVisibilityResponse.class);
  }

  @Override
  public IRunnerResponse update(SectionVisibilityRequest request) {
    SectionVisibility sectionVisibility = repository.findById(request.getId())
        .orElseThrow(() -> new SectionVisibilityException(
            "SectionVisibility not found with ID: " + request.getId()));

    sectionVisibility.setBranch(request.getBranch());
    sectionVisibility.setMode(request.getMode());
    sectionVisibility.setDirection(request.getDirection());

    // Validate and Update SectionDetails mappings
    if (request.getSectionDetailIds() != null) {
      Set<SectionDetails> sectionDetails = validateAndGetSectionDetails(
          request.getSectionDetailIds());
      sectionVisibility.setSections(sectionDetails);
    }

    return modelMapper.map(repository.save(sectionVisibility), SectionVisibilityResponse.class);
  }

  @Override
  public IRunnerResponse delete(Long id) {
    SectionVisibility sectionVisibility = repository.findById(id)
        .orElseThrow(
            () -> new SectionVisibilityException("SectionVisibility not found with ID: " + id));

    // Validation: Prevent deletion if linked to SectionDetails
    if (!sectionVisibility.getSections().isEmpty()) {
      throw new SectionVisibilityException(
          "Cannot delete. SectionVisibility is linked to SectionDetails.");
    }

    repository.deleteById(id);
    return modelMapper.map(sectionVisibility, SectionVisibilityResponse.class);
  }

  @Override
  public IRunnerResponse findByTenantIdModeDirection(Integer tenantId, String mode,
      String direction) {
    SectionVisibility sectionVisibility = repository.findByTenantIdAndModeAndDirection(tenantId,
        mode, direction);
    if (ObjectUtils.isEmpty(sectionVisibility)) {
      throw new SectionVisibilityException(
          "SectionVisibility with the tenantId, mode, and direction does not exists.");
    }
    SectionVisibilityResponse sectionVisibilityResponse = modelMapper.map(sectionVisibility,
        SectionVisibilityResponse.class);
    sectionVisibilityResponse.setSectionDetailsResponses(sectionVisibility.getSections().stream()
        .map(sectionDetails -> {
          SectionDetailsResponse sectionDetailsResponse = modelMapper.map(sectionDetails,
              SectionDetailsResponse.class);
          sectionDetailsResponse.setSectionFieldsResponses(
              sectionDetails.getSectionFields().stream()
                  .map(sectionFields -> modelMapper.map(sectionFields,
                      SectionFieldsResponse.class)).collect(Collectors.toList()));
          return sectionDetailsResponse;
        })
        .collect(
            Collectors.toSet()));
    return sectionVisibilityResponse;

  }

  @Override
  public IRunnerResponse getById(Long id) {
    SectionVisibility sectionVisibility = repository.findById(id)
        .orElseThrow(
            () -> new SectionVisibilityException("SectionVisibility not found with ID: " + id));

    SectionVisibilityResponse sectionVisibilityResponse = modelMapper.map(sectionVisibility,
        SectionVisibilityResponse.class);
    sectionVisibilityResponse.setSectionDetailsResponses(sectionVisibility.getSections().stream()
        .map(sectionFields -> modelMapper.map(sectionFields, SectionDetailsResponse.class))
        .collect(Collectors.toSet()));
    return sectionVisibilityResponse;
  }

  @Override
  public IRunnerResponse getAll() {
    List<SectionVisibility> entities = repository.findAll();
    List<SectionVisibilityResponse> sectionDetailsResponses = entities.stream()
        .map(entity -> {
          SectionVisibilityResponse sectionVisibilityResponse = modelMapper.map(entity,
              SectionVisibilityResponse.class);
          sectionVisibilityResponse.setSectionDetailsResponses(entity.getSections().stream()
              .map(sectionFields -> modelMapper.map(sectionFields, SectionDetailsResponse.class))
              .collect(Collectors.toSet()));
          return sectionVisibilityResponse;
        })
        .collect(Collectors.toList());

    RunnerListResponse runnerListResponse = new RunnerListResponse();
    runnerListResponse.setData(sectionDetailsResponses);
    runnerListResponse.setSuccess(true);
    return runnerListResponse;
  }

  /**
   * Validates if all SectionDetail IDs exist and returns the corresponding entities.
   */
  private Set<SectionDetails> validateAndGetSectionDetails(Set<Long> sectionDetailIds) {
    List<SectionDetails> sectionDetails = sectionDetailsRepository.findAllById(sectionDetailIds);

    if (sectionDetails.size() != sectionDetailIds.size()) {
      throw new SectionVisibilityException("One or more provided SectionDetail IDs are invalid.");
    }

    return new HashSet<>(sectionDetails);
  }
}
