package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.section.request.SectionFieldsRequest;
import com.dpw.runner.shipment.services.dto.section.response.SectionFieldsResponse;
import com.dpw.runner.shipment.services.entity.SectionFields;
import com.dpw.runner.shipment.services.exception.exceptions.SectionFieldsException;
import com.dpw.runner.shipment.services.repository.interfaces.ISectionFieldsRepository;
import com.dpw.runner.shipment.services.service.interfaces.ISectionFieldsService;
import java.util.List;
import java.util.stream.Collectors;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SectionFieldsServiceImpl implements ISectionFieldsService {

  @Autowired
  private ISectionFieldsRepository sectionFieldsRepository;

  @Autowired
  private ModelMapper modelMapper;

  @Override
  public SectionFieldsResponse create(SectionFieldsRequest request) {
    if (sectionFieldsRepository.existsByFieldName(request.getFieldName())) {
      throw new IllegalArgumentException("Field name already exists: " + request.getFieldName());
    }
    SectionFields field = modelMapper.map(request, SectionFields.class);
    field = sectionFieldsRepository.save(field);
    return modelMapper.map(field, SectionFieldsResponse.class);
  }

  @Override
  public SectionFieldsResponse update(SectionFieldsRequest request) {
    SectionFields field = sectionFieldsRepository.findById(request.getId())
        .orElseThrow(() -> new SectionFieldsException("SectionFields not found."));

    modelMapper.map(request, field);
    field = sectionFieldsRepository.save(field);
    return modelMapper.map(field, SectionFieldsResponse.class);
  }

  @Override
  public IRunnerResponse delete(Long id) {
    SectionFields sectionField = sectionFieldsRepository.findById(id)
        .orElseThrow(() -> new SectionFieldsException("SectionField not found with ID: " + id));

    // Check if this SectionField is linked to any SectionDetails
    if (!sectionField.getSectionDetails().isEmpty()) {
      throw new IllegalStateException("Cannot delete. SectionField is linked to SectionDetails.");
    }
    sectionFieldsRepository.deleteById(id);
    return modelMapper.map(sectionField, SectionFieldsResponse.class);
  }

  @Override
  public SectionFieldsResponse getById(Long id) {
    SectionFields field = sectionFieldsRepository.findById(id)
        .orElseThrow(() -> new SectionFieldsException("SectionFields not found."));
    return modelMapper.map(field, SectionFieldsResponse.class);
  }

  @Override
  public RunnerListResponse getAll() {
    List<SectionFieldsResponse> fieldsResponses = sectionFieldsRepository.findAll().stream()
        .map(field -> modelMapper.map(field, SectionFieldsResponse.class))
        .collect(Collectors.toList());
    RunnerListResponse runnerListResponse = new RunnerListResponse();
    runnerListResponse.setData(fieldsResponses);
    runnerListResponse.setSuccess(true);
    return runnerListResponse;
  }
}
