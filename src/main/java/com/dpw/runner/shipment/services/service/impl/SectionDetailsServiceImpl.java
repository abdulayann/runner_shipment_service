package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.SectionDetailsConstant;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.section.request.SectionDetailsRequest;
import com.dpw.runner.shipment.services.dto.section.response.SectionDetailsResponse;
import com.dpw.runner.shipment.services.dto.section.response.SectionFieldsResponse;
import com.dpw.runner.shipment.services.entity.SectionDetails;
import com.dpw.runner.shipment.services.entity.SectionFields;
import com.dpw.runner.shipment.services.exception.exceptions.SectionDetailsException;
import com.dpw.runner.shipment.services.repository.interfaces.ISectionDetailsRepository;
import com.dpw.runner.shipment.services.repository.interfaces.ISectionFieldsRepository;
import com.dpw.runner.shipment.services.service.interfaces.ISectionDetailsService;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

@Service
public class SectionDetailsServiceImpl implements ISectionDetailsService {

  private final ISectionDetailsRepository sectionDetailsRepository;
  private final ISectionFieldsRepository sectionFieldsRepository;
  private final ModelMapper modelMapper;

  public SectionDetailsServiceImpl(ISectionDetailsRepository sectionDetailsRepository,
      ISectionFieldsRepository sectionFieldsRepository, ModelMapper modelMapper) {
    this.sectionDetailsRepository = sectionDetailsRepository;
    this.sectionFieldsRepository = sectionFieldsRepository;
    this.modelMapper = modelMapper;
  }

  @Override
  public SectionDetailsResponse create(SectionDetailsRequest request) {

    if (sectionDetailsRepository.existsBySectionName(request.getSectionName())) {
      throw new SectionDetailsException(
          SectionDetailsConstant.SECTION_ALREADY_EXIST + request.getSectionName());
    }
    SectionDetails entity = modelMapper.map(request, SectionDetails.class);

    // Validate SectionFields
    Set<SectionFields> sectionFields = validateAndFetchSectionFields(request.getSectionFieldIds());
    entity.setSectionFields(sectionFields);

    SectionDetails savedEntity = sectionDetailsRepository.save(entity);
    return modelMapper.map(savedEntity, SectionDetailsResponse.class);
  }

  private Set<SectionFields> validateAndFetchSectionFields(List<Long> sectionFieldIds) {
    if (sectionFieldIds == null || sectionFieldIds.isEmpty()) {
      throw new SectionDetailsException(SectionDetailsConstant.ATLEAST_ONE_SECTION_FIELD_REQUIRED);
    }

    List<SectionFields> sectionFields = sectionFieldsRepository.findAllById(sectionFieldIds);
    if (sectionFields.size() != sectionFieldIds.size()) {
      throw new SectionDetailsException(SectionDetailsConstant.SECTION_FIELD_INVALID_IDS);
    }

    return new HashSet<>(sectionFields);
  }

  @Override
  public SectionDetailsResponse update(SectionDetailsRequest request) {
    if (ObjectUtils.isEmpty(request.getId())) {
      throw new SectionDetailsException(SectionDetailsConstant.ID_CAN_NOT_NULL);
    }
    SectionDetails existingEntity = sectionDetailsRepository.findById(request.getId())
        .orElseThrow(
            () -> new SectionDetailsException(SectionDetailsConstant.NOT_FOUND + request.getId()));

    existingEntity.setSectionName(request.getSectionName());
    existingEntity.setSectionDescription(request.getSectionDescription());
    // Validate and set SectionFields
    Set<SectionFields> sectionFields = validateAndFetchSectionFields(request.getSectionFieldIds());
    existingEntity.setSectionFields(sectionFields);

    SectionDetails updatedEntity = sectionDetailsRepository.save(existingEntity);
    return modelMapper.map(updatedEntity, SectionDetailsResponse.class);
  }

  @Override
  public IRunnerResponse delete(Long id) {
    SectionDetails sectionDetails = sectionDetailsRepository.findById(id)
        .orElseThrow(() -> new SectionDetailsException(SectionDetailsConstant.NOT_FOUND + id));

    // Check if SectionDetails is linked to any SectionVisibility
    if (!sectionDetails.getSectionVisibilities().isEmpty()) {
      throw new SectionDetailsException(SectionDetailsConstant.CAN_NOT_DELETE_LINK_EXIST);
    }
    sectionDetailsRepository.deleteById(id);
    return modelMapper.map(sectionDetails, SectionDetailsResponse.class);
  }

  @Override
  public SectionDetailsResponse getById(Long id) {
    SectionDetails entity = sectionDetailsRepository.findById(id)
        .orElseThrow(() -> new SectionDetailsException(SectionDetailsConstant.NOT_FOUND + id));

    SectionDetailsResponse response = modelMapper.map(entity, SectionDetailsResponse.class);
    response.setSectionFieldsResponses(entity.getSectionFields().stream()
        .map(sectionFields -> modelMapper.map(sectionFields, SectionFieldsResponse.class))
        .toList());

    return response;
  }

  @Override
  public IRunnerResponse getAll() {
    List<SectionDetails> entities = sectionDetailsRepository.findAll();
    List<SectionDetailsResponse> sectionDetailsResponses = entities.stream()
        .map(entity -> {
          SectionDetailsResponse sectionDetailsResponse = modelMapper.map(entity,
              SectionDetailsResponse.class);
          sectionDetailsResponse.setSectionFieldsResponses(entity.getSectionFields().stream()
              .map(sectionFields -> modelMapper.map(sectionFields, SectionFieldsResponse.class))
              .toList());
          return sectionDetailsResponse;
        })
        .toList();

    RunnerListResponse<SectionDetailsResponse> runnerListResponse = new RunnerListResponse<>();
    runnerListResponse.setData(sectionDetailsResponses);
    runnerListResponse.setSuccess(true);
    return runnerListResponse;
  }
}

