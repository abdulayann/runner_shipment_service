package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyIterable;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.commons.constants.SectionDetailsConstant;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.section.request.SectionDetailsRequest;
import com.dpw.runner.shipment.services.dto.section.response.SectionDetailsResponse;
import com.dpw.runner.shipment.services.entity.SectionDetails;
import com.dpw.runner.shipment.services.entity.SectionFields;
import com.dpw.runner.shipment.services.entity.SectionVisibility;
import com.dpw.runner.shipment.services.exception.exceptions.SectionDetailsException;
import com.dpw.runner.shipment.services.repository.interfaces.ISectionDetailsRepository;
import com.dpw.runner.shipment.services.repository.interfaces.ISectionFieldsRepository;
import java.time.LocalDate;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import com.dpw.runner.shipment.services.repository.interfaces.ISectionVisibilityRepository;
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
class SectionDetailsServiceImplTest {

  @Mock
  private ISectionDetailsRepository iSectionDetailsRepository;

  @Mock
  private ISectionVisibilityRepository sectionVisibilityRepository;

  @Mock
  private ModelMapper modelMapper;

  @Mock
  private ISectionFieldsRepository sectionFieldsRepository;

  @InjectMocks
  private SectionDetailsServiceImpl sectionDetailsService;


  @Test
  void testCreate() {
    when(iSectionDetailsRepository.existsBySectionName(Mockito.<String>any())).thenReturn(true);
    SectionDetailsRequest request = new SectionDetailsRequest();
    assertThrows(SectionDetailsException.class,
        () -> sectionDetailsService.create(request));
    verify(iSectionDetailsRepository).existsBySectionName(Mockito.<String>any());
  }

  @Test
  void testCreateException() {
    when(iSectionDetailsRepository.existsBySectionName(Mockito.<String>any())).thenReturn(false);
    SectionDetailsRequest request = new SectionDetailsRequest();
    assertThrows(SectionDetailsException.class,
        () -> sectionDetailsService.create(request));
    verify(iSectionDetailsRepository).existsBySectionName(Mockito.<String>any());
  }

  @Test
  void testDelete() {
    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setSectionName("Section Description");
    sectionDetails.setSectionDescription("Section Name");
    sectionDetails.setGuid(UUID.randomUUID());
    sectionDetails.setId(1L);
    sectionDetails.setIsDeleted(true);
    sectionDetails.setSectionFields(new HashSet<>());
    sectionDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setUpdatedBy("2020-03-01");
    Optional<SectionDetails> ofResult = Optional.of(sectionDetails);
    doNothing().when(iSectionDetailsRepository).deleteById(Mockito.<Long>any());
    when(iSectionDetailsRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
    SectionDetailsResponse buildResult = SectionDetailsResponse.builder()
        .sectionDescription("Section Description")
        .sectionName("section Name")
        .id(1L)
        .build();
    when(modelMapper.map(Mockito.<Object>any(),
        Mockito.<Class<SectionDetailsResponse>>any())).thenReturn(buildResult);
    IRunnerResponse actualDeleteResult = sectionDetailsService.delete(1L);
    verify(modelMapper).map(Mockito.<Object>any(), Mockito.<Class<SectionDetailsResponse>>any());
    verify(iSectionDetailsRepository).deleteById(Mockito.<Long>any());
    verify(iSectionDetailsRepository).findById(Mockito.<Long>any());
    assertTrue(actualDeleteResult instanceof SectionDetailsResponse);
  }

  /**
   * Method under test: {@link SectionDetailsServiceImpl#delete(Long)}
   */
  @Test
  void testDelete2() {
    when(iSectionDetailsRepository.findById(Mockito.<Long>any())).thenReturn(Optional.empty());
    SectionDetailsException exception = assertThrows(SectionDetailsException.class,
        () -> sectionDetailsService.delete(1L));
    verify(iSectionDetailsRepository).findById(Mockito.<Long>any());
    assertEquals(SectionDetailsConstant.NOT_FOUND + 1, exception.getMessage());
  }

  /**
   * Method under test: {@link SectionDetailsServiceImpl#delete(Long)}
   */
  @Test
  void testDelete3() {
    SectionVisibility sectionVisibility = new SectionVisibility("Branch", "SEA", "IMP","187883",
        new HashSet<>());
    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setSectionName("Section Description");
    sectionDetails.setSectionDescription("Section Name");
    sectionDetails.setGuid(UUID.randomUUID());
    sectionDetails.setId(1L);
    sectionDetails.setIsDeleted(true);
    sectionDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setUpdatedBy("2020-03-01");

    Optional<SectionDetails> ofResult = Optional.of(sectionDetails);
    when(iSectionDetailsRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
    when(sectionVisibilityRepository.findBySectionDetailsId(anyLong())).thenReturn(List.of(sectionVisibility));
    SectionDetailsException exception = assertThrows(SectionDetailsException.class,
        () -> sectionDetailsService.delete(1L));
    verify(iSectionDetailsRepository).findById(Mockito.<Long>any());
    assertEquals(SectionDetailsConstant.CAN_NOT_DELETE_LINK_EXIST, exception.getMessage());
  }

  @Test
  void testGetByIdException() {
    Optional<SectionDetails> emptyResult = Optional.empty();
    when(iSectionDetailsRepository.findById(Mockito.<Long>any())).thenReturn(emptyResult);
    assertThrows(SectionDetailsException.class, () -> sectionDetailsService.getById(1L));
    verify(iSectionDetailsRepository).findById(Mockito.<Long>any());
  }

  @Test
  void testGetByIdSuccess() {
    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setSectionName("Section Description");
    sectionDetails.setSectionDescription("Section Name");
    sectionDetails.setGuid(UUID.randomUUID());
    sectionDetails.setId(1L);
    sectionDetails.setIsDeleted(true);
    sectionDetails.setSectionFields(new HashSet<>());
    sectionDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setUpdatedBy("2020-03-01");
    Optional<SectionDetails> ofResult = Optional.of(sectionDetails);
    SectionDetailsResponse buildResult = SectionDetailsResponse.builder()
        .sectionDescription("Section Description")
        .sectionName("section Name")
        .id(1L)
        .build();
    when(modelMapper.map(Mockito.<Object>any(),
        Mockito.<Class<SectionDetailsResponse>>any())).thenReturn(buildResult);
    when(iSectionDetailsRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
    sectionDetailsService.getById(1L);
    verify(iSectionDetailsRepository).findById(Mockito.<Long>any());
    verify(modelMapper).map(Mockito.<Object>any(), Mockito.<Class<SectionDetailsResponse>>any());
  }

  @Test
  void testGetAll() {
    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setSectionName("Section Description");
    sectionDetails.setSectionDescription("Section Name");
    sectionDetails.setGuid(UUID.randomUUID());
    sectionDetails.setId(1L);
    sectionDetails.setIsDeleted(true);
    sectionDetails.setSectionFields(new HashSet<>());
    sectionDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setUpdatedBy("2020-03-01");
    SectionDetailsResponse buildResult = SectionDetailsResponse.builder()
        .sectionDescription("Section Description")
        .sectionName("section Name")
        .id(1L)
        .build();
    when(modelMapper.map(Mockito.<Object>any(),
        Mockito.<Class<SectionDetailsResponse>>any())).thenReturn(buildResult);
    when(iSectionDetailsRepository.findAll()).thenReturn(List.of(sectionDetails));
    sectionDetailsService.getAll();
    verify(iSectionDetailsRepository).findAll();
    verify(modelMapper).map(Mockito.<Object>any(), Mockito.<Class<SectionDetailsResponse>>any());
  }

  @Test
  void testUpdateException() {
    SectionDetailsRequest sectionDetailsRequest = new SectionDetailsRequest();
    sectionDetailsRequest.setSectionDescription("Section Description");
    sectionDetailsRequest.setSectionName("Section Name");
    SectionDetailsException exception = assertThrows(SectionDetailsException.class,
        () -> sectionDetailsService.update(sectionDetailsRequest));
    assertEquals(SectionDetailsConstant.ID_CAN_NOT_NULL, exception.getMessage());
    verify(modelMapper, never()).map(any(), any());
  }

  @Test
  void testUpdateException1() {
    SectionDetailsRequest sectionDetailsRequest = new SectionDetailsRequest();
    sectionDetailsRequest.setId(1L);
    sectionDetailsRequest.setSectionDescription("Section Description");
    sectionDetailsRequest.setSectionName("Section Name");
    when(iSectionDetailsRepository.findById(any())).thenReturn(Optional.empty());
    SectionDetailsException exception = assertThrows(SectionDetailsException.class,
        () -> sectionDetailsService.update(sectionDetailsRequest));
    verify(iSectionDetailsRepository).findById(Mockito.<Long>any());
    assertEquals(SectionDetailsConstant.NOT_FOUND + sectionDetailsRequest.getId(),
        exception.getMessage());
    verify(modelMapper, never()).map(any(), any());
  }

  @Test
  void testUpdate() {

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

    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setSectionName("Section Description");
    sectionDetails.setSectionDescription("Section Name");
    sectionDetails.setGuid(UUID.randomUUID());
    sectionDetails.setId(1L);
    sectionDetails.setIsDeleted(true);
    sectionDetails.setSectionFields(Set.of(sectionFields));
    sectionDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setUpdatedBy("2020-03-01");

    SectionDetailsRequest sectionDetailsRequest = new SectionDetailsRequest();
    sectionDetailsRequest.setId(1L);
    sectionDetailsRequest.setSectionDescription("Section Description");
    sectionDetailsRequest.setSectionName("Section Name");
    sectionDetailsRequest.setSectionFieldIds(List.of(1L));

    SectionDetailsResponse buildResult = SectionDetailsResponse.builder()
        .sectionDescription("Section Description")
        .sectionName("section Name")
        .id(1L)
        .build();
    when(sectionFieldsRepository.findAllById(anyIterable())).thenReturn(List.of(sectionFields));
    when(iSectionDetailsRepository.findById(anyLong())).thenReturn(Optional.of(sectionDetails));
    when(iSectionDetailsRepository.save(sectionDetails)).thenReturn(sectionDetails);
    when(modelMapper.map(sectionDetails, SectionDetailsResponse.class)).thenReturn(buildResult);
    sectionDetailsService.update(sectionDetailsRequest);
    verify(iSectionDetailsRepository).save(any());
    verify(modelMapper, times(1)).map(any(), any());
  }

  @Test
  void testCreateValidation() {
    SectionDetailsRequest sectionDetailsRequest = new SectionDetailsRequest();
    sectionDetailsRequest.setId(1L);
    sectionDetailsRequest.setSectionDescription("Section Description");
    sectionDetailsRequest.setSectionName("Section Name");
    sectionDetailsRequest.setSectionFieldIds(List.of(1L,2L));

    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setSectionDescription("Section Description");
    sectionDetails.setSectionName("Section Name");
    sectionDetails.setGuid(UUID.randomUUID());
    sectionDetails.setId(1L);
    sectionDetails.setIsDeleted(true);
    sectionDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setUpdatedBy("2020-03-01");
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
    when(iSectionDetailsRepository.existsBySectionName(any())).thenReturn(false);
    when(sectionFieldsRepository.findAllById(anyIterable())).thenReturn(List.of(sectionFields));
    when(modelMapper.map(sectionDetailsRequest, SectionDetails.class)).thenReturn(sectionDetails);
    SectionDetailsException exception = assertThrows(SectionDetailsException.class,
        () -> sectionDetailsService.create(sectionDetailsRequest));
    verify(iSectionDetailsRepository).existsBySectionName(anyString());
    assertEquals(SectionDetailsConstant.SECTION_FIELD_INVALID_IDS,
        exception.getMessage());
  }
  @Test
  void testCreateSuccess() {
    SectionDetailsRequest sectionDetailsRequest = new SectionDetailsRequest();
    sectionDetailsRequest.setId(1L);
    sectionDetailsRequest.setSectionDescription("Section Description");
    sectionDetailsRequest.setSectionName("Section Name");
    sectionDetailsRequest.setSectionFieldIds(List.of(1L));

    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setSectionDescription("Section Description");
    sectionDetails.setSectionName("Section Name");
    sectionDetails.setGuid(UUID.randomUUID());
    sectionDetails.setId(1L);
    sectionDetails.setIsDeleted(true);
    sectionDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setUpdatedBy("2020-03-01");
    SectionDetailsResponse buildResult = SectionDetailsResponse.builder()
        .sectionDescription("Section Description")
        .sectionName("section Name")
        .id(1L)
        .build();
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
    when(iSectionDetailsRepository.existsBySectionName(any())).thenReturn(false);
    when(sectionFieldsRepository.findAllById(anyIterable())).thenReturn(List.of(sectionFields));
    when(modelMapper.map(sectionDetailsRequest, SectionDetails.class)).thenReturn(sectionDetails);
    when(iSectionDetailsRepository.save(sectionDetails)).thenReturn(sectionDetails);
    when(modelMapper.map(sectionDetails, SectionDetailsResponse.class)).thenReturn(buildResult);
    sectionDetailsService.create(sectionDetailsRequest);
    verify(iSectionDetailsRepository).existsBySectionName(anyString());
    verify(iSectionDetailsRepository).save(any());
    verify(modelMapper, times(2)).map(any(), any());
  }
}
