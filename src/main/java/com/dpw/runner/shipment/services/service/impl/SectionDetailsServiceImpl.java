package com.dpw.runner.shipment.services.service.impl;

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
import java.util.stream.Collectors;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SectionDetailsServiceImpl implements ISectionDetailsService {

  @Autowired
  private ISectionDetailsRepository sectionDetailsRepository;
  @Autowired
  private ISectionFieldsRepository sectionFieldsRepository;
  @Autowired
  private ModelMapper modelMapper;

  @Override
  public SectionDetailsResponse create(SectionDetailsRequest request) {

    if (sectionDetailsRepository.existsBySectionName(request.getSectionName())) {
      throw new IllegalArgumentException(
          "Section name already exists: " + request.getSectionName());
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
      throw new SectionDetailsException("At least one SectionField ID is required!");
    }

    List<SectionFields> sectionFields = sectionFieldsRepository.findAllById(sectionFieldIds);
    if (sectionFields.size() != sectionFieldIds.size()) {
      throw new SectionDetailsException("One or more SectionField IDs are invalid!");
    }

    return new HashSet<>(sectionFields);
  }

  @Override
  public SectionDetailsResponse update(SectionDetailsRequest request) {
    SectionDetails existingEntity = sectionDetailsRepository.findById(request.getId())
        .orElseThrow(() -> new SectionDetailsException("SectionDetails not found!"));

    modelMapper.map(request, existingEntity);

    // Validate and set SectionFields
    Set<SectionFields> sectionFields = validateAndFetchSectionFields(request.getSectionFieldIds());
    existingEntity.setSectionFields(sectionFields);

    SectionDetails updatedEntity = sectionDetailsRepository.save(existingEntity);
    return modelMapper.map(updatedEntity, SectionDetailsResponse.class);
  }

  @Override
  public IRunnerResponse delete(Long id) {
    SectionDetails sectionDetails = sectionDetailsRepository.findById(id)
        .orElseThrow(() -> new IllegalArgumentException("SectionDetails not found with ID: " + id));

    // Check if SectionDetails is linked to any SectionVisibility
    if (!sectionDetails.getSectionVisibilities().isEmpty()) {
      throw new IllegalStateException(
          "Cannot delete. SectionDetails is linked to SectionVisibility.");
    }
    sectionDetailsRepository.deleteById(id);
    return modelMapper.map(sectionDetails, SectionDetailsResponse.class);
  }

  @Override
  public SectionDetailsResponse getById(Long id) {
    SectionDetails entity = sectionDetailsRepository.findById(id)
        .orElseThrow(() -> new SectionDetailsException("SectionDetails not found!"));

    SectionDetailsResponse response = modelMapper.map(entity, SectionDetailsResponse.class);
    response.setSectionFieldsResponses(entity.getSectionFields().stream()
        .map(sectionFields -> modelMapper.map(sectionFields, SectionFieldsResponse.class))
        .collect(Collectors.toList()));

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
              .collect(Collectors.toList()));
          return sectionDetailsResponse;
        })
        .collect(Collectors.toList());

    RunnerListResponse runnerListResponse = new RunnerListResponse();
    runnerListResponse.setData(sectionDetailsResponses);
    runnerListResponse.setSuccess(true);
    return runnerListResponse;
  }
}

