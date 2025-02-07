package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.SectionVisibilityConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.section.request.SectionVisibilityRequest;
import com.dpw.runner.shipment.services.dto.section.response.SectionDetailsResponse;
import com.dpw.runner.shipment.services.dto.section.response.SectionFieldsResponse;
import com.dpw.runner.shipment.services.dto.section.response.SectionVisibilityResponse;
import com.dpw.runner.shipment.services.entity.SectionDetails;
import com.dpw.runner.shipment.services.entity.SectionVisibility;
import com.dpw.runner.shipment.services.exception.exceptions.SectionVisibilityException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ISectionDetailsRepository;
import com.dpw.runner.shipment.services.repository.interfaces.ISectionVisibilityRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.ISectionVisibilityService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

@Service
@Slf4j
public class SectionVisibilityServiceImpl implements ISectionVisibilityService {

  private final ISectionVisibilityRepository repository;
  private final ISectionDetailsRepository sectionDetailsRepository;
  private final ModelMapper modelMapper;
  private final IV1Service v1Service;
  private final JsonHelper jsonHelper;
  private final IAuditLogService auditLogService;

  public SectionVisibilityServiceImpl(ISectionVisibilityRepository repository,
      ISectionDetailsRepository sectionDetailsRepository, ModelMapper modelMapper,
      IV1Service v1Service, JsonHelper jsonHelper, IAuditLogService auditLogService) {
    this.repository = repository;
    this.sectionDetailsRepository = sectionDetailsRepository;
    this.modelMapper = modelMapper;
    this.v1Service = v1Service;
    this.jsonHelper = jsonHelper;
    this.auditLogService = auditLogService;
  }

  @Override
  public IRunnerResponse create(SectionVisibilityRequest request) {
    // Check for unique constraint violation
    if (repository.existsByBranchCodeAndModeAndDirection(request.getBranchCode(), request.getMode(),
        request.getDirection())) {
      throw new SectionVisibilityException(SectionVisibilityConstants.ALREADY_EXIST);
    }
    TenantModel tenantDetails = getTenantDetails();
    if (!request.getBranchCode().equals(tenantDetails.getCode())) {
      throw new SectionVisibilityException(SectionVisibilityConstants.BRANCH_MISMATCHED);
    }
    SectionVisibility sectionVisibility = modelMapper.map(request, SectionVisibility.class);

    // Validate and Map SectionDetails
    if (request.getSectionDetailIds() != null && !request.getSectionDetailIds().isEmpty()) {
      Set<SectionDetails> sectionDetails = validateAndGetSectionDetails(
          request.getSectionDetailIds());
      sectionVisibility.setSections(sectionDetails);
    }
    sectionVisibility = repository.save(sectionVisibility);
    try {
      auditLogService.addAuditLog(
          AuditLogMetaData.builder()
              .tenantId(UserContext.getUser().getTenantId())
              .userName(UserContext.getUser().Username)
              .newData(sectionVisibility)
              .prevData(null)
              .parent(SectionVisibility.class.getSimpleName())
              .parentId(sectionVisibility.getId())
              .operation(DBOperationType.CREATE.name()).build());
    } catch (Exception ex) {
      log.error(SectionVisibilityConstants.CREATE_AUDIT_LOG_ERROR_MESSAGE, ex.getMessage(), ex);
    }
    return modelMapper.map(sectionVisibility, SectionVisibilityResponse.class);
  }

  private TenantModel getTenantDetails() {
    return modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
  }

  @Override
  public IRunnerResponse update(SectionVisibilityRequest request) {
    if (ObjectUtils.isEmpty(request.getId())) {
      throw new SectionVisibilityException(SectionVisibilityConstants.ID_CAN_NOT_BE_NULL);
    }
    TenantModel tenantDetails = getTenantDetails();
    if (!request.getBranchCode().equals(tenantDetails.getCode())) {
      throw new SectionVisibilityException(SectionVisibilityConstants.BRANCH_MISMATCHED);
    }
    SectionVisibility sectionVisibility = repository.findById(request.getId())
        .orElseThrow(() -> new SectionVisibilityException(
            SectionVisibilityConstants.NOT_FOUND + request.getId()));
    SectionVisibility oldEntity = jsonHelper.convertValue(sectionVisibility,
        SectionVisibility.class);
    sectionVisibility.setBranchCode(request.getBranchCode());
    sectionVisibility.setMode(request.getMode());
    sectionVisibility.setDirection(request.getDirection());

    // Validate and Update SectionDetails mappings
    if (request.getSectionDetailIds() != null) {
      Set<SectionDetails> sectionDetails = validateAndGetSectionDetails(
          request.getSectionDetailIds());
      sectionVisibility.setSections(sectionDetails);
    }
    sectionVisibility = repository.save(sectionVisibility);
    try {
      auditLogService.addAuditLog(
          AuditLogMetaData.builder()
              .tenantId(UserContext.getUser().getTenantId())
              .userName(UserContext.getUser().Username)
              .newData(sectionVisibility)
              .prevData(oldEntity)
              .parent(SectionVisibility.class.getSimpleName())
              .parentId(sectionVisibility.getId())
              .operation(DBOperationType.UPDATE.name()).build());
    } catch (Exception ex) {
      log.error(SectionVisibilityConstants.UPDATE_AUDIT_LOG_ERROR_MESSAGE, ex.getMessage(), ex);
    }
    return modelMapper.map(sectionVisibility, SectionVisibilityResponse.class);
  }

  @Override
  public IRunnerResponse delete(Long id) {
    SectionVisibility sectionVisibility = repository.findById(id)
        .orElseThrow(
            () -> new SectionVisibilityException(SectionVisibilityConstants.NOT_FOUND + id));
    repository.deleteById(id);
    return modelMapper.map(sectionVisibility, SectionVisibilityResponse.class);
  }

  @Override
  public IRunnerResponse findByBranchModeDirection(String branch, String mode,
      String direction) {
    SectionVisibility sectionVisibility = repository.findByBranchCodeAndModeAndDirection(branch,
        mode, direction);
    if (ObjectUtils.isEmpty(sectionVisibility)) {
      throw new SectionVisibilityException(SectionVisibilityConstants.ENTITY_DOES_NOT_EXIST);
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
                      SectionFieldsResponse.class)).toList());
          return sectionDetailsResponse;
        })
        .collect(
            Collectors.toSet()));
    return sectionVisibilityResponse;

  }

  @Override
  public IRunnerResponse getAllPaginated(int page, int size, String sortBy,
      String sortDirection) {

    Sort sort = sortDirection.equalsIgnoreCase("desc") ? Sort.by(sortBy).descending()
        : Sort.by(sortBy).ascending();
    Pageable pageable = PageRequest.of(page - 1, size, sort);

    Page<SectionVisibility> sectionVisibilityPage = repository.findAll(pageable);

    List<SectionVisibilityResponse> sectionDetailsResponses = getSectionVisibilityResponses(
        sectionVisibilityPage.getContent());
    RunnerListResponse<SectionVisibilityResponse> runnerListResponse = new RunnerListResponse<>();
    runnerListResponse.setTotalPages(sectionVisibilityPage.getTotalPages());
    runnerListResponse.setNumberOfRecords(sectionVisibilityPage.getTotalElements());
    runnerListResponse.setData(sectionDetailsResponses);
    runnerListResponse.setSuccess(true);
    return runnerListResponse;
  }

  private List<SectionVisibilityResponse> getSectionVisibilityResponses(
      List<SectionVisibility> sectionVisibilityPage) {
    return sectionVisibilityPage.stream()
        .map(entity -> {
          SectionVisibilityResponse sectionVisibilityResponse = modelMapper.map(entity,
              SectionVisibilityResponse.class);
          sectionVisibilityResponse.setSectionDetailsResponses(entity.getSections().stream()
              .map(sectionDetails -> {
                SectionDetailsResponse sectionDetailsResponse = modelMapper.map(sectionDetails,
                    SectionDetailsResponse.class);
                sectionDetailsResponse.setSectionFieldsResponses(
                    sectionDetails.getSectionFields().stream().map(
                        sectionFields -> modelMapper.map(sectionFields,
                            SectionFieldsResponse.class)).toList());
                return sectionDetailsResponse;
              })
              .collect(Collectors.toSet()));
          return sectionVisibilityResponse;
        })
        .toList();
  }

  @Override
  public List<SectionVisibilityResponse> getByBranchCode(String code) {
    List<SectionVisibility> sectionVisibilities = repository.findByBranchCode(code);
    return getSectionVisibilityResponses(sectionVisibilities);
  }

  @Override
  public IRunnerResponse getById(Long id) {
    SectionVisibility sectionVisibility = repository.findById(id)
        .orElseThrow(
            () -> new SectionVisibilityException(SectionVisibilityConstants.NOT_FOUND + id));

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
        .toList();

    RunnerListResponse<SectionVisibilityResponse> runnerListResponse = new RunnerListResponse<>();
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
      throw new SectionVisibilityException(SectionVisibilityConstants.INVALID_IDS);
    }

    return new HashSet<>(sectionDetails);
  }
}
