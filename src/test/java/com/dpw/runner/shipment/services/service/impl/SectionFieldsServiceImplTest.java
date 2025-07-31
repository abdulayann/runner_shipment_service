package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.commons.constants.SectionFieldsConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.section.request.SectionFieldsRequest;
import com.dpw.runner.shipment.services.dto.section.response.SectionFieldsResponse;
import com.dpw.runner.shipment.services.entity.SectionDetails;
import com.dpw.runner.shipment.services.entity.SectionFields;
import com.dpw.runner.shipment.services.exception.exceptions.SectionFieldsException;
import com.dpw.runner.shipment.services.repository.interfaces.ISectionFieldsRepository;
import java.time.LocalDate;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class SectionFieldsServiceImplTest {

  @Mock
  private ISectionFieldsRepository iSectionFieldsRepository;

  @Mock
  private ModelMapper modelMapper;

  @InjectMocks
  private SectionFieldsServiceImpl sectionFieldsServiceImpl;


  @Test
  void testCreate() {
    when(iSectionFieldsRepository.existsByFieldName(Mockito.<String>any())).thenReturn(true);
    SectionFieldsRequest request = new SectionFieldsRequest();
    assertThrows(SectionFieldsException.class,
        () -> sectionFieldsServiceImpl.create(request));
    verify(iSectionFieldsRepository).existsByFieldName(Mockito.<String>any());
  }

  @Test
  void testDelete() {
    SectionFields sectionFields = new SectionFields();
    sectionFields.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionFields.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionFields.setFieldDescription("Field Description");
    sectionFields.setFieldName("Field Name");
    sectionFields.setGuid(UUID.randomUUID());
    sectionFields.setId(1L);
    sectionFields.setIsDeleted(true);
    sectionFields.setSectionDetails(new HashSet<>());
    sectionFields.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionFields.setUpdatedBy("2020-03-01");
    Optional<SectionFields> ofResult = Optional.of(sectionFields);
    doNothing().when(iSectionFieldsRepository).deleteById(Mockito.<Long>any());
    when(iSectionFieldsRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
    SectionFieldsResponse buildResult = SectionFieldsResponse.builder()
        .fieldDescription("Field Description")
        .fieldName("Field Name")
        .id(1L)
        .build();
    when(modelMapper.map(Mockito.<Object>any(),
        Mockito.<Class<SectionFieldsResponse>>any())).thenReturn(buildResult);
    IRunnerResponse actualDeleteResult = sectionFieldsServiceImpl.delete(1L);
    verify(modelMapper).map(Mockito.<Object>any(), Mockito.<Class<SectionFieldsResponse>>any());
    verify(iSectionFieldsRepository).deleteById(Mockito.<Long>any());
    verify(iSectionFieldsRepository).findById(Mockito.<Long>any());
    assertTrue(actualDeleteResult instanceof SectionFieldsResponse);
  }

  @Test
  void testDelete2() {
    SectionFields sectionFields = new SectionFields();
    sectionFields.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionFields.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionFields.setFieldDescription("Field Description");
    sectionFields.setFieldName("Field Name");
    sectionFields.setGuid(UUID.randomUUID());
    sectionFields.setId(1L);
    sectionFields.setIsDeleted(true);
    sectionFields.setSectionDetails(new HashSet<>());
    sectionFields.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionFields.setUpdatedBy("2020-03-01");
    Optional<SectionFields> ofResult = Optional.of(sectionFields);
    doNothing().when(iSectionFieldsRepository).deleteById(Mockito.<Long>any());
    when(iSectionFieldsRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
    when(modelMapper.map(Mockito.<Object>any(), Mockito.<Class<SectionFieldsResponse>>any()))
        .thenThrow(new IllegalArgumentException("foo"));
    assertThrows(IllegalArgumentException.class, () -> sectionFieldsServiceImpl.delete(1L));
    verify(modelMapper).map(Mockito.<Object>any(), Mockito.<Class<SectionFieldsResponse>>any());
    verify(iSectionFieldsRepository).deleteById(Mockito.<Long>any());
    verify(iSectionFieldsRepository).findById(Mockito.<Long>any());
  }

  @Test
  void testDelete3() {
    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setGuid(UUID.randomUUID());
    sectionDetails.setId(1L);
    sectionDetails.setIsDeleted(true);
    sectionDetails.setSectionName("Section Description");
    sectionDetails.setSectionFields(new HashSet<>());
    sectionDetails.setSectionCode("Section Name");
    sectionDetails.setSectionVisibilities(new HashSet<>());
    sectionDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setUpdatedBy("2020-03-01");

    HashSet<SectionDetails> sectionDetails2 = new HashSet<>();
    sectionDetails2.add(sectionDetails);

    SectionFields sectionFields = new SectionFields();
    sectionFields.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionFields.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionFields.setFieldDescription("Field Description");
    sectionFields.setFieldName("Field Name");
    sectionFields.setGuid(UUID.randomUUID());
    sectionFields.setId(1L);
    sectionFields.setIsDeleted(true);
    sectionFields.setSectionDetails(sectionDetails2);
    sectionFields.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionFields.setUpdatedBy("2020-03-01");
    Optional<SectionFields> ofResult = Optional.of(sectionFields);
    when(iSectionFieldsRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
    assertThrows(SectionFieldsException.class, () -> sectionFieldsServiceImpl.delete(1L));
    verify(iSectionFieldsRepository).findById(Mockito.<Long>any());
  }

  @Test
  void testDelete4() {
    Optional<SectionFields> emptyResult = Optional.empty();
    when(iSectionFieldsRepository.findById(Mockito.<Long>any())).thenReturn(emptyResult);
    assertThrows(SectionFieldsException.class, () -> sectionFieldsServiceImpl.delete(1L));
    verify(iSectionFieldsRepository).findById(Mockito.<Long>any());
  }

  @Test
  void testGetByIdException() {
    Optional<SectionFields> emptyResult = Optional.empty();
    when(iSectionFieldsRepository.findById(Mockito.<Long>any())).thenReturn(emptyResult);
    assertThrows(SectionFieldsException.class, () -> sectionFieldsServiceImpl.getById(1L));
    verify(iSectionFieldsRepository).findById(Mockito.<Long>any());
  }

  @Test
  void testGetByIdSuccess() {
    SectionFields sectionFields = new SectionFields();
    sectionFields.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionFields.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionFields.setFieldDescription("Field Description");
    sectionFields.setFieldName("Field Name");
    sectionFields.setGuid(UUID.randomUUID());
    sectionFields.setId(1L);
    sectionFields.setIsDeleted(true);
    sectionFields.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionFields.setUpdatedBy("2020-03-01");
    Optional<SectionFields> ofResult = Optional.of(sectionFields);
    SectionFieldsResponse buildResult = SectionFieldsResponse.builder()
        .fieldDescription("Field Description")
        .fieldName("Field Name")
        .id(1L)
        .build();
    when(modelMapper.map(Mockito.<Object>any(),
        Mockito.<Class<SectionFieldsResponse>>any())).thenReturn(buildResult);
    when(iSectionFieldsRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
    sectionFieldsServiceImpl.getById(1L);
    verify(iSectionFieldsRepository).findById(Mockito.<Long>any());
    verify(modelMapper).map(Mockito.<Object>any(), Mockito.<Class<SectionFieldsResponse>>any());
  }

  @Test
  void testGetAll() {
    SectionFields sectionFields = new SectionFields();
    sectionFields.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionFields.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionFields.setFieldDescription("Field Description");
    sectionFields.setFieldName("Field Name");
    sectionFields.setGuid(UUID.randomUUID());
    sectionFields.setId(1L);
    sectionFields.setIsDeleted(true);
    sectionFields.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionFields.setUpdatedBy("2020-03-01");
    SectionFieldsResponse buildResult = SectionFieldsResponse.builder()
        .fieldDescription("Field Description")
        .fieldName("Field Name")
        .id(1L)
        .build();
    when(modelMapper.map(Mockito.<Object>any(),
        Mockito.<Class<SectionFieldsResponse>>any())).thenReturn(buildResult);
    when(iSectionFieldsRepository.findAll()).thenReturn(List.of(sectionFields));
    sectionFieldsServiceImpl.getAll();
    verify(iSectionFieldsRepository).findAll();
    verify(modelMapper).map(Mockito.<Object>any(), Mockito.<Class<SectionFieldsResponse>>any());
  }

  @Test
  void testUpdateException() {
    SectionFieldsRequest sectionFieldsRequest = new SectionFieldsRequest();
    sectionFieldsRequest.setFieldDescription("Field Description");
    sectionFieldsRequest.setFieldName("Field Name");
    SectionFieldsException exception = assertThrows(SectionFieldsException.class,
        () -> sectionFieldsServiceImpl.update(sectionFieldsRequest));
    assertEquals(SectionFieldsConstants.ID_NOT_NULL, exception.getMessage());
    verify(modelMapper, never()).map(any(), any());
  }

  @Test
  void testUpdateException1() {
    SectionFieldsRequest sectionFieldsRequest = new SectionFieldsRequest();
    sectionFieldsRequest.setId(1L);
    sectionFieldsRequest.setFieldDescription("Field Description");
    sectionFieldsRequest.setFieldName("Field Name");
    when(iSectionFieldsRepository.findById(any())).thenReturn(Optional.empty());
    SectionFieldsException exception = assertThrows(SectionFieldsException.class,
        () -> sectionFieldsServiceImpl.update(sectionFieldsRequest));
    verify(iSectionFieldsRepository).findById(Mockito.<Long>any());
    assertEquals(SectionFieldsConstants.NOT_FOUND + sectionFieldsRequest.getId(),
        exception.getMessage());
    verify(modelMapper, never()).map(any(), any());
  }

  @Test
  void testUpdate() {
    SectionFieldsRequest sectionFieldsRequest = new SectionFieldsRequest();
    sectionFieldsRequest.setId(1L);
    sectionFieldsRequest.setFieldDescription("Field Description");
    sectionFieldsRequest.setFieldName("Field Name");
    SectionFields sectionFields = new SectionFields();
    sectionFields.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionFields.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionFields.setFieldDescription("Field Description");
    sectionFields.setFieldName("Field Name");
    sectionFields.setGuid(UUID.randomUUID());
    sectionFields.setId(1L);
    sectionFields.setIsDeleted(true);
    sectionFields.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionFields.setUpdatedBy("2020-03-01");
    Optional<SectionFields> ofResult = Optional.of(sectionFields);
    SectionFieldsResponse buildResult = SectionFieldsResponse.builder()
        .fieldDescription("Field Description")
        .fieldName("Field Name")
        .id(1L)
        .build();
    when(iSectionFieldsRepository.findById(any())).thenReturn(ofResult);
    when(iSectionFieldsRepository.save(sectionFields)).thenReturn(sectionFields);
    when(modelMapper.map(Mockito.<Object>any(),
        Mockito.<Class<SectionFieldsResponse>>any())).thenReturn(buildResult);
    sectionFieldsServiceImpl.update(sectionFieldsRequest);
    verify(iSectionFieldsRepository).findById(Mockito.<Long>any());
    verify(iSectionFieldsRepository).save(any());
    verify(modelMapper).map(Mockito.<Object>any(), Mockito.<Class<SectionFieldsResponse>>any());
  }

  @Test
  void testCreateSuccess() {
    SectionFieldsRequest sectionFieldsRequest = new SectionFieldsRequest();
    sectionFieldsRequest.setId(1L);
    sectionFieldsRequest.setFieldDescription("Field Description");
    sectionFieldsRequest.setFieldName("Field Name");
    SectionFields sectionFields = new SectionFields();
    sectionFields.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionFields.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionFields.setFieldDescription("Field Description");
    sectionFields.setFieldName("Field Name");
    sectionFields.setGuid(UUID.randomUUID());
    sectionFields.setId(1L);
    sectionFields.setIsDeleted(true);
    sectionFields.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionFields.setUpdatedBy("2020-03-01");
    SectionFieldsResponse buildResult = SectionFieldsResponse.builder()
        .fieldDescription("Field Description")
        .fieldName("Field Name")
        .id(1L)
        .build();
    when(iSectionFieldsRepository.existsByFieldName(any())).thenReturn(false);
    when(modelMapper.map(sectionFieldsRequest, SectionFields.class)).thenReturn(sectionFields);
    when(iSectionFieldsRepository.save(sectionFields)).thenReturn(sectionFields);
    when(modelMapper.map(sectionFields, SectionFieldsResponse.class)).thenReturn(buildResult);
    sectionFieldsServiceImpl.create(sectionFieldsRequest);
    verify(iSectionFieldsRepository).existsByFieldName(anyString());
    verify(iSectionFieldsRepository).save(any());
    verify(modelMapper, times(2)).map(any(), any());
  }
}
