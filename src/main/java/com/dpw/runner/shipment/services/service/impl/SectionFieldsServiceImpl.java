package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.SectionFieldsConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.section.request.SectionFieldsRequest;
import com.dpw.runner.shipment.services.dto.section.response.SectionFieldsResponse;
import com.dpw.runner.shipment.services.entity.SectionFields;
import com.dpw.runner.shipment.services.exception.exceptions.SectionFieldsException;
import com.dpw.runner.shipment.services.repository.interfaces.ISectionFieldsRepository;
import com.dpw.runner.shipment.services.service.interfaces.ISectionFieldsService;
import java.util.List;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

@Service
public class SectionFieldsServiceImpl implements ISectionFieldsService {

  private final ISectionFieldsRepository sectionFieldsRepository;
  private final ModelMapper modelMapper;

  public SectionFieldsServiceImpl(ISectionFieldsRepository sectionFieldsRepository,
      ModelMapper modelMapper) {
    this.sectionFieldsRepository = sectionFieldsRepository;
    this.modelMapper = modelMapper;
  }

  @Override
  public SectionFieldsResponse create(SectionFieldsRequest request) {
    if (sectionFieldsRepository.existsByFieldName(request.getFieldName())) {
      throw new SectionFieldsException(
          SectionFieldsConstants.FIELD_ALREADY_EXIST + request.getFieldName());
    }
    SectionFields field = modelMapper.map(request, SectionFields.class);
    field = sectionFieldsRepository.save(field);
    return modelMapper.map(field, SectionFieldsResponse.class);
  }

  @Override
  public SectionFieldsResponse update(SectionFieldsRequest request) {
    if (ObjectUtils.isEmpty(request.getId())) {
      throw new SectionFieldsException(SectionFieldsConstants.ID_NOT_NULL);
    }
    SectionFields field = sectionFieldsRepository.findById(request.getId())
        .orElseThrow(
            () -> new SectionFieldsException(SectionFieldsConstants.NOT_FOUND + request.getId()));
    field.setFieldDescription(request.getFieldDescription());
    field.setFieldName(request.getFieldName());
    field = sectionFieldsRepository.save(field);
    return modelMapper.map(field, SectionFieldsResponse.class);
  }

  @Override
  public IRunnerResponse delete(Long id) {
    SectionFields sectionField = sectionFieldsRepository.findById(id)
        .orElseThrow(() -> new SectionFieldsException(SectionFieldsConstants.NOT_FOUND + id));

    // Check if this SectionField is linked to any SectionDetails
    if (!sectionField.getSectionDetails().isEmpty()) {
      throw new SectionFieldsException(SectionFieldsConstants.LINK_EXIST_CAN_NOT_DELETE);
    }
    sectionFieldsRepository.deleteById(id);
    return modelMapper.map(sectionField, SectionFieldsResponse.class);
  }

  @Override
  public SectionFieldsResponse getById(Long id) {
    SectionFields field = sectionFieldsRepository.findById(id)
        .orElseThrow(() -> new SectionFieldsException(SectionFieldsConstants.NOT_FOUND + id));
    return modelMapper.map(field, SectionFieldsResponse.class);
  }

  @Override
  public RunnerListResponse getAll() {
    List<SectionFieldsResponse> fieldsResponses = sectionFieldsRepository.findAll().stream()
        .map(field -> modelMapper.map(field, SectionFieldsResponse.class))
        .toList();
    RunnerListResponse<SectionFieldsResponse> runnerListResponse = new RunnerListResponse<>();
    runnerListResponse.setData(fieldsResponses);
    runnerListResponse.setSuccess(true);
    return runnerListResponse;
  }
}
