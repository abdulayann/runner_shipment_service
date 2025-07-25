package com.dpw.runner.shipment.services.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyIterable;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.SectionVisibilityConstants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.section.request.SectionVisibilityRequest;
import com.dpw.runner.shipment.services.dto.section.response.SectionDetailsResponse;
import com.dpw.runner.shipment.services.dto.section.response.SectionFieldsResponse;
import com.dpw.runner.shipment.services.dto.section.response.SectionVisibilityResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.entity.SectionDetails;
import com.dpw.runner.shipment.services.entity.SectionFields;
import com.dpw.runner.shipment.services.entity.SectionVisibility;
import com.dpw.runner.shipment.services.exception.exceptions.SectionVisibilityException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ISectionDetailsRepository;
import com.dpw.runner.shipment.services.repository.interfaces.ISectionVisibilityRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import java.time.LocalDate;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class SectionVisibilityServiceImplTest {

  @Mock
  private ISectionDetailsRepository iSectionDetailsRepository;

  @Mock
  private ModelMapper modelMapper;

  @Mock
  private ISectionVisibilityRepository sectionVisibilityRepository;

  @Mock
  private IV1Service v1Service;

  @Mock
  private IAuditLogService auditLogService;

  @Mock
  private JsonHelper jsonHelper;

  @InjectMocks
  private SectionVisibilityServiceImpl sectionVisibilityService;


  @Test
  void testCreateAlreadyExistValidation() {
    SectionVisibility sectionVisibility = new SectionVisibility();
    sectionVisibility.setId(1L);
    when(sectionVisibilityRepository.existsByBranchCodeAndModeAndDirection(anyString(), anyString(),
        anyString())).thenReturn(true);
    SectionVisibilityRequest request = new SectionVisibilityRequest(1L, "Branch", "SEA", "IMP","187883", new HashSet<>());
    SectionVisibilityException exception = assertThrows(SectionVisibilityException.class,
        () -> sectionVisibilityService.create(request));
    verify(sectionVisibilityRepository).existsByBranchCodeAndModeAndDirection(anyString(), anyString(),
        anyString());
    assertEquals(SectionVisibilityConstants.ALREADY_EXIST, exception.getMessage());
  }

  @Test
  void testCreateBranchMismatchedValidation() {
    when(sectionVisibilityRepository.existsByBranchCodeAndModeAndDirection(anyString(), anyString(),
        anyString())).thenReturn(false);
    TenantModel tenantModel = new TenantModel();
    tenantModel.setCode("Test");
    when(v1Service.retrieveTenant()).thenReturn(new V1RetrieveResponse());
    when(modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class)).thenReturn(
        tenantModel);
    SectionVisibilityRequest request = new SectionVisibilityRequest(1L, "Branch", "SEA", "IMP", "187883", new HashSet<>());
    SectionVisibilityException exception = assertThrows(SectionVisibilityException.class,
        () -> sectionVisibilityService.create(request));
    verify(sectionVisibilityRepository).existsByBranchCodeAndModeAndDirection(anyString(), anyString(),
        anyString());
    assertEquals(SectionVisibilityConstants.BRANCH_MISMATCHED, exception.getMessage());
  }

  @Test
  void testCreateValidation() {

    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setSectionName("Section Description");
    sectionDetails.setSectionCode("Section Name");
    sectionDetails.setGuid(UUID.randomUUID());
    sectionDetails.setId(1L);
    sectionDetails.setIsDeleted(true);
    sectionDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setUpdatedBy("2020-03-01");

    SectionVisibility sectionVisibility = new SectionVisibility();
    sectionVisibility.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionVisibility.setBranchCode("Test");
    sectionVisibility.setMode("SEA");
    sectionVisibility.setDirection("IMP");
    sectionVisibility.setGuid(UUID.randomUUID());
    sectionVisibility.setId(1L);
    sectionVisibility.setIsDeleted(true);
    sectionVisibility.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setUpdatedBy("2020-03-01");
    SectionVisibilityRequest sectionVisibilityRequest = new SectionVisibilityRequest();
    sectionVisibilityRequest.setId(1L);
    sectionVisibilityRequest.setBranchCode("Test");
    sectionVisibilityRequest.setDirection("IMP");
    sectionVisibilityRequest.setMode("SEA");
    sectionVisibilityRequest.setSectionDetailIds(Set.of(1L, 2L));

    when(sectionVisibilityRepository.existsByBranchCodeAndModeAndDirection(anyString(), anyString(),
        anyString())).thenReturn(false);
    TenantModel tenantModel = new TenantModel();
    tenantModel.setCode("Test");
    when(v1Service.retrieveTenant()).thenReturn(new V1RetrieveResponse());
    when(modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class)).thenReturn(
        tenantModel);
    when(modelMapper.map(sectionVisibilityRequest, SectionVisibility.class)).thenReturn(
        sectionVisibility);
    when(iSectionDetailsRepository.findAllById(anyIterable())).thenReturn(List.of(sectionDetails));

    SectionVisibilityException exception = assertThrows(SectionVisibilityException.class,
        () -> sectionVisibilityService.create(sectionVisibilityRequest));
    verify(sectionVisibilityRepository).existsByBranchCodeAndModeAndDirection(anyString(), anyString(),
        anyString());
    assertEquals(SectionVisibilityConstants.INVALID_IDS,
        exception.getMessage());
  }

  @Test
  void testCreateSuccess() {

    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setSectionName("Section Description");
    sectionDetails.setSectionCode("Section Name");
    sectionDetails.setGuid(UUID.randomUUID());
    sectionDetails.setId(1L);
    sectionDetails.setIsDeleted(true);
    sectionDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setUpdatedBy("2020-03-01");

    SectionVisibility sectionVisibility = new SectionVisibility();
    sectionVisibility.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionVisibility.setBranchCode("Test");
    sectionVisibility.setMode("SEA");
    sectionVisibility.setDirection("IMP");
    sectionVisibility.setGuid(UUID.randomUUID());
    sectionVisibility.setId(1L);
    sectionVisibility.setIsDeleted(true);
    sectionVisibility.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setUpdatedBy("2020-03-01");

    SectionVisibilityResponse buildResult = SectionVisibilityResponse.builder()
        .branch("Test")
        .mode("SEA")
        .direction("IMP")
        .id(1L)
        .build();

    SectionVisibilityRequest sectionVisibilityRequest = new SectionVisibilityRequest();
    sectionVisibilityRequest.setId(1L);
    sectionVisibilityRequest.setBranchCode("Test");
    sectionVisibilityRequest.setDirection("IMP");
    sectionVisibilityRequest.setMode("SEA");
    sectionVisibilityRequest.setSectionDetailIds(Set.of(1L));

    when(sectionVisibilityRepository.existsByBranchCodeAndModeAndDirection(anyString(), anyString(),
        anyString())).thenReturn(false);
    TenantModel tenantModel = new TenantModel();
    tenantModel.setCode("Test");
    when(v1Service.retrieveTenant()).thenReturn(new V1RetrieveResponse());
    when(modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class)).thenReturn(
        tenantModel);
    when(modelMapper.map(sectionVisibilityRequest, SectionVisibility.class)).thenReturn(
        sectionVisibility);
    when(iSectionDetailsRepository.findAllById(anyIterable())).thenReturn(List.of(sectionDetails));
    when(sectionVisibilityRepository.save(sectionVisibility)).thenReturn(sectionVisibility);
    when(modelMapper.map(sectionVisibility, SectionVisibilityResponse.class)).thenReturn(
        buildResult);
    UsersDto usersDto = new UsersDto();
    usersDto.setTenantId(1);
    UserContext.setUser(usersDto);
    sectionVisibilityService.create(sectionVisibilityRequest);
    verify(sectionVisibilityRepository).save(any());
    verify(modelMapper, times(3)).map(any(), any());
    verify(sectionVisibilityRepository).existsByBranchCodeAndModeAndDirection(anyString(), anyString(),
        anyString());
  }

  @Test
  void testDelete() {
    SectionVisibility sectionVisibility = new SectionVisibility();
    sectionVisibility.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionVisibility.setBranchCode("Test");
    sectionVisibility.setMode("SEA");
    sectionVisibility.setDirection("IMP");
    sectionVisibility.setGuid(UUID.randomUUID());
    sectionVisibility.setId(1L);
    sectionVisibility.setIsDeleted(true);
    sectionVisibility.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setUpdatedBy("2020-03-01");
    Optional<SectionVisibility> ofResult = Optional.of(sectionVisibility);

    SectionVisibilityResponse buildResult = SectionVisibilityResponse.builder()
        .branch("Test")
        .mode("SEA")
        .direction("IMP")
        .id(1L)
        .build();

    doNothing().when(sectionVisibilityRepository).deleteById(Mockito.<Long>any());
    when(sectionVisibilityRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);

    when(modelMapper.map(Mockito.<Object>any(),
        Mockito.<Class<SectionVisibilityResponse>>any())).thenReturn(buildResult);
    IRunnerResponse actualDeleteResult = sectionVisibilityService.delete(1L);
    verify(modelMapper).map(Mockito.<Object>any(), Mockito.<Class<SectionDetailsResponse>>any());
    verify(sectionVisibilityRepository).deleteById(Mockito.<Long>any());
    verify(sectionVisibilityRepository).findById(Mockito.<Long>any());
    assertTrue(actualDeleteResult instanceof SectionVisibilityResponse);
  }

  /**
   * Method under test: {@link SectionDetailsServiceImpl#delete(Long)}
   */
  @Test
  void testDelete2() {
    when(sectionVisibilityRepository.findById(Mockito.<Long>any())).thenReturn(Optional.empty());
    SectionVisibilityException exception = assertThrows(SectionVisibilityException.class,
        () -> sectionVisibilityService.delete(1L));
    verify(sectionVisibilityRepository).findById(Mockito.<Long>any());
    assertEquals(SectionVisibilityConstants.NOT_FOUND + 1, exception.getMessage());
  }

  @Test
  void testGetByIdException() {
    Optional<SectionVisibility> emptyResult = Optional.empty();
    when(sectionVisibilityRepository.findById(Mockito.<Long>any())).thenReturn(emptyResult);
    assertThrows(SectionVisibilityException.class, () -> sectionVisibilityService.getById(1L));
    verify(sectionVisibilityRepository).findById(Mockito.<Long>any());
  }

  @Test
  void testGetByIdSuccess() {
    SectionVisibility sectionVisibility = new SectionVisibility();
    sectionVisibility.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionVisibility.setBranchCode("Test");
    sectionVisibility.setMode("SEA");
    sectionVisibility.setDirection("IMP");
    sectionVisibility.setGuid(UUID.randomUUID());
    sectionVisibility.setId(1L);
    sectionVisibility.setIsDeleted(true);
    sectionVisibility.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setUpdatedBy("2020-03-01");
    Optional<SectionVisibility> ofResult = Optional.of(sectionVisibility);
    SectionVisibilityResponse buildResult = SectionVisibilityResponse.builder()
        .branch("Test")
        .mode("SEA")
        .direction("IMP")
        .id(1L)
        .build();
    when(modelMapper.map(Mockito.<Object>any(),
        Mockito.<Class<SectionVisibilityResponse>>any())).thenReturn(buildResult);
    when(sectionVisibilityRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
    sectionVisibilityService.getById(1L);
    verify(sectionVisibilityRepository).findById(Mockito.<Long>any());
    verify(modelMapper).map(Mockito.<Object>any(), Mockito.<Class<SectionDetailsResponse>>any());
  }

  @Test
  void testFindByBranchModeDirection() {
    SectionVisibility sectionVisibility = new SectionVisibility();
    sectionVisibility.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionVisibility.setBranchCode("Test");
    sectionVisibility.setMode("SEA");
    sectionVisibility.setDirection("IMP");
    sectionVisibility.setGuid(UUID.randomUUID());
    sectionVisibility.setId(1L);
    sectionVisibility.setIsDeleted(true);
    sectionVisibility.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setUpdatedBy("2020-03-01");

    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setSectionName("Section Description");
    sectionDetails.setSectionCode("Section Name");
    sectionDetails.setGuid(UUID.randomUUID());
    sectionDetails.setId(1L);
    sectionDetails.setIsDeleted(true);
    sectionDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setUpdatedBy("2020-03-01");
    sectionVisibility.setSections(Set.of(sectionDetails));
    SectionVisibilityResponse buildResult = SectionVisibilityResponse.builder()
        .branch("Test")
        .mode("SEA")
        .direction("IMP")
        .id(1L)
        .build();
    SectionDetailsResponse buildResult1 = SectionDetailsResponse.builder()
        .sectionCode("Section Description")
        .sectionName("section Name")
        .id(1L)
        .build();
    when(modelMapper.map(any(SectionVisibility.class),
        Mockito.<Class<SectionVisibilityResponse>>any())).thenReturn(buildResult);
    when(modelMapper.map(any(SectionDetails.class),
        Mockito.<Class<SectionDetailsResponse>>any())).thenReturn(buildResult1);
    when(sectionVisibilityRepository.findByBranchCodeAndModeAndDirection(anyString(), anyString(),
        anyString())).thenReturn(sectionVisibility);
    sectionVisibilityService.findByBranchModeDirection("Test", "SEA", "IMP");
    verify(sectionVisibilityRepository).findByBranchCodeAndModeAndDirection(anyString(), anyString(),
        anyString());
    verify(modelMapper, times(2)).map(any(), any());
  }

  @Test
  void testFindByBranchModeDirectionException() {

    when(sectionVisibilityRepository.findByBranchCodeAndModeAndDirection(anyString(), anyString(),
        anyString())).thenReturn(null);
    SectionVisibilityException exception = assertThrows(SectionVisibilityException.class,
        () -> sectionVisibilityService.findByBranchModeDirection("Test", "SEA", "IMP"));
    verify(sectionVisibilityRepository).findByBranchCodeAndModeAndDirection(anyString(), anyString(),
        anyString());
    assertEquals(SectionVisibilityConstants.ENTITY_DOES_NOT_EXIST, exception.getMessage());
  }

  @Test
  void testGetAll() {
    SectionVisibility sectionVisibility = new SectionVisibility();
    sectionVisibility.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionVisibility.setBranchCode("Test");
    sectionVisibility.setMode("SEA");
    sectionVisibility.setDirection("IMP");
    sectionVisibility.setGuid(UUID.randomUUID());
    sectionVisibility.setId(1L);
    sectionVisibility.setIsDeleted(true);
    sectionVisibility.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setUpdatedBy("2020-03-01");
    SectionVisibilityResponse buildResult = SectionVisibilityResponse.builder()
        .branch("Test")
        .mode("SEA")
        .direction("IMP")
        .id(1L)
        .build();
    when(modelMapper.map(Mockito.<Object>any(),
        Mockito.<Class<SectionVisibilityResponse>>any())).thenReturn(buildResult);
    when(sectionVisibilityRepository.findAll()).thenReturn(List.of(sectionVisibility));
    sectionVisibilityService.getAll();
    verify(sectionVisibilityRepository).findAll();
    verify(modelMapper).map(Mockito.<Object>any(), Mockito.<Class<SectionDetailsResponse>>any());
  }

  @Test
  void getAllPaginatedDescOrder() {
    SectionVisibility sectionVisibility = new SectionVisibility();
    sectionVisibility.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionVisibility.setBranchCode("Test");
    sectionVisibility.setMode("SEA");
    sectionVisibility.setDirection("IMP");
    sectionVisibility.setGuid(UUID.randomUUID());
    sectionVisibility.setId(1L);
    sectionVisibility.setIsDeleted(true);
    sectionVisibility.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setUpdatedBy("2020-03-01");

    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setSectionName("Section Description");
    sectionDetails.setSectionCode("Section Name");
    sectionDetails.setGuid(UUID.randomUUID());
    sectionDetails.setId(1L);
    sectionDetails.setIsDeleted(true);
    sectionDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setUpdatedBy("2020-03-01");
    sectionVisibility.setSections(Set.of(sectionDetails));
    SectionVisibilityResponse buildResult = SectionVisibilityResponse.builder()
        .branch("Test")
        .mode("SEA")
        .direction("IMP")
        .id(1L)
        .build();
    SectionDetailsResponse buildResult1 = SectionDetailsResponse.builder()
        .sectionCode("Section Description")
        .sectionName("section Name")
        .id(1L)
        .build();

    when(modelMapper.map(any(SectionVisibility.class),
        Mockito.<Class<SectionVisibilityResponse>>any())).thenReturn(buildResult);
    when(modelMapper.map(any(SectionDetails.class),
        Mockito.<Class<SectionDetailsResponse>>any())).thenReturn(buildResult1);
    List<SectionVisibility> sectionVisibilities = List.of(sectionVisibility);
    Pageable pageable = PageRequest.of(0, 1); // Page 0, size 10
    Page<SectionVisibility> mockPage = new PageImpl<>(sectionVisibilities, pageable,
        sectionVisibilities.size());

    when(sectionVisibilityRepository.findAll(any(Pageable.class))).thenReturn(mockPage);
    sectionVisibilityService.getAllPaginated(1, 1, "id", "desc");
    verify(sectionVisibilityRepository).findAll(any(Pageable.class));
    verify(modelMapper, times(2)).map(any(), any());
  }

  @Test
  void getAllPaginatedAscOrder() {
    SectionVisibility sectionVisibility = new SectionVisibility();
    sectionVisibility.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionVisibility.setBranchCode("Test");
    sectionVisibility.setMode("SEA");
    sectionVisibility.setDirection("IMP");
    sectionVisibility.setGuid(UUID.randomUUID());
    sectionVisibility.setId(1L);
    sectionVisibility.setIsDeleted(true);
    sectionVisibility.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setUpdatedBy("2020-03-01");

    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setSectionName("Section Description");
    sectionDetails.setSectionCode("Section Name");
    sectionDetails.setGuid(UUID.randomUUID());
    sectionDetails.setId(1L);
    sectionDetails.setIsDeleted(true);
    sectionDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setUpdatedBy("2020-03-01");
    sectionVisibility.setSections(Set.of(sectionDetails));
    SectionVisibilityResponse buildResult = SectionVisibilityResponse.builder()
        .branch("Test")
        .mode("SEA")
        .direction("IMP")
        .id(1L)
        .build();
    SectionDetailsResponse buildResult1 = SectionDetailsResponse.builder()
        .sectionCode("Section Description")
        .sectionName("section Name")
        .id(1L)
        .build();

    when(modelMapper.map(any(SectionVisibility.class),
        Mockito.<Class<SectionVisibilityResponse>>any())).thenReturn(buildResult);
    when(modelMapper.map(any(SectionDetails.class),
        Mockito.<Class<SectionDetailsResponse>>any())).thenReturn(buildResult1);
    List<SectionVisibility> sectionVisibilities = List.of(sectionVisibility);
    Pageable pageable = PageRequest.of(0, 1); // Page 0, size 10
    Page<SectionVisibility> mockPage = new PageImpl<>(sectionVisibilities, pageable,
        sectionVisibilities.size());

    when(sectionVisibilityRepository.findAll(any(Pageable.class))).thenReturn(mockPage);
    sectionVisibilityService.getAllPaginated(1, 1, "id", "asc");
    verify(sectionVisibilityRepository).findAll(any(Pageable.class));
    verify(modelMapper, times(2)).map(any(), any());
  }

  @Test
  void getAllPaginated() {
    SectionVisibility sectionVisibility = new SectionVisibility();
    sectionVisibility.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionVisibility.setBranchCode("Test");
    sectionVisibility.setMode("SEA");
    sectionVisibility.setDirection("IMP");
    sectionVisibility.setGuid(UUID.randomUUID());
    sectionVisibility.setId(1L);
    sectionVisibility.setIsDeleted(true);
    sectionVisibility.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setUpdatedBy("2020-03-01");

    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setSectionName("Section Description");
    sectionDetails.setSectionCode("Section Name");
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

    sectionDetails.setSectionFields(Set.of(sectionFields));
    SectionFieldsResponse buildResult2 = SectionFieldsResponse.builder()
        .fieldDescription("Field Description")
        .fieldName("Field Name")
        .id(1L)
        .build();

    sectionVisibility.setSections(Set.of(sectionDetails));
    SectionVisibilityResponse buildResult = SectionVisibilityResponse.builder()
        .branch("Test")
        .mode("SEA")
        .direction("IMP")
        .id(1L)
        .build();
    SectionDetailsResponse buildResult1 = SectionDetailsResponse.builder()
        .sectionCode("Section Description")
        .sectionName("section Name")
        .id(1L)
        .build();

    when(modelMapper.map(any(SectionVisibility.class),
        Mockito.<Class<SectionVisibilityResponse>>any())).thenReturn(buildResult);
    when(modelMapper.map(any(SectionDetails.class),
        Mockito.<Class<SectionDetailsResponse>>any())).thenReturn(buildResult1);
    when(modelMapper.map(any(SectionFields.class),
        Mockito.<Class<SectionFieldsResponse>>any())).thenReturn(buildResult2);
    List<SectionVisibility> sectionVisibilities = List.of(sectionVisibility);
    Pageable pageable = PageRequest.of(0, 1); // Page 0, size 10
    Page<SectionVisibility> mockPage = new PageImpl<>(sectionVisibilities, pageable,
        sectionVisibilities.size());

    when(sectionVisibilityRepository.findAll(any(Pageable.class))).thenReturn(mockPage);
    sectionVisibilityService.getAllPaginated(1, 1, "id", "asc");
    verify(sectionVisibilityRepository).findAll(any(Pageable.class));
    verify(modelMapper, times(3)).map(any(), any());
  }

  @Test
  void testUpdateIdNullValidation() {
    SectionVisibilityRequest request = new SectionVisibilityRequest(null, "Branch", "SEA", "IMP", "187883", new HashSet<>());
    SectionVisibilityException exception = assertThrows(SectionVisibilityException.class,
        () -> sectionVisibilityService.update(request));
    assertEquals(SectionVisibilityConstants.ID_CAN_NOT_BE_NULL, exception.getMessage());
  }

  @Test
  void testUpdateBranchValidation() {
    SectionVisibility sectionVisibility = new SectionVisibility();
    sectionVisibility.setId(1L);

    TenantModel tenantModel = new TenantModel();
    tenantModel.setCode("Test");
    when(v1Service.retrieveTenant()).thenReturn(new V1RetrieveResponse());
    when(modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class)).thenReturn(
        tenantModel);
    SectionVisibilityRequest request = new SectionVisibilityRequest(1L, "Branch", "SEA", "IMP", "187883", new HashSet<>());
    SectionVisibilityException exception = assertThrows(SectionVisibilityException.class,
        () -> sectionVisibilityService.update(request));
    assertEquals(SectionVisibilityConstants.BRANCH_MISMATCHED, exception.getMessage());
  }

  @Test
  void testUpdateIdNotValidValidation() {
    TenantModel tenantModel = new TenantModel();
    tenantModel.setCode("Test");
    when(v1Service.retrieveTenant()).thenReturn(new V1RetrieveResponse());
    when(modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class)).thenReturn(
        tenantModel);
    when(sectionVisibilityRepository.findById(anyLong())).thenReturn(Optional.empty());
    SectionVisibilityRequest request = new SectionVisibilityRequest(1L, "Test", "SEA", "IMP", "187883", new HashSet<>());
    SectionVisibilityException exception = assertThrows(SectionVisibilityException.class,
        () -> sectionVisibilityService.update(request));
    assertEquals(SectionVisibilityConstants.NOT_FOUND + 1, exception.getMessage());
  }

  @Test
  void testUpdateSuccess() {

    SectionDetails sectionDetails = new SectionDetails();
    sectionDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionDetails.setSectionName("Section Description");
    sectionDetails.setSectionCode("Section Name");
    sectionDetails.setGuid(UUID.randomUUID());
    sectionDetails.setId(1L);
    sectionDetails.setIsDeleted(true);
    sectionDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionDetails.setUpdatedBy("2020-03-01");

    SectionVisibility sectionVisibility = new SectionVisibility();
    sectionVisibility.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionVisibility.setBranchCode("Test");
    sectionVisibility.setMode("SEA");
    sectionVisibility.setDirection("IMP");
    sectionVisibility.setGuid(UUID.randomUUID());
    sectionVisibility.setId(1L);
    sectionVisibility.setIsDeleted(true);
    sectionVisibility.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setUpdatedBy("2020-03-01");
    SectionVisibilityResponse buildResult = SectionVisibilityResponse.builder()
        .branch("Test")
        .mode("SEA")
        .direction("IMP")
        .id(1L)
        .build();

    SectionVisibilityRequest sectionVisibilityRequest = new SectionVisibilityRequest();
    sectionVisibilityRequest.setId(1L);
    sectionVisibilityRequest.setBranchCode("Test");
    sectionVisibilityRequest.setDirection("IMP");
    sectionVisibilityRequest.setMode("SEA");
    sectionVisibilityRequest.setSectionDetailIds(Set.of(1L));

    TenantModel tenantModel = new TenantModel();
    tenantModel.setCode("Test");
    when(v1Service.retrieveTenant()).thenReturn(new V1RetrieveResponse());
    when(modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class)).thenReturn(
        tenantModel);
    when(jsonHelper.convertValue(sectionVisibility, SectionVisibility.class)).thenReturn(
        sectionVisibility);
    when(sectionVisibilityRepository.findById(anyLong())).thenReturn(
        Optional.of(sectionVisibility));
    when(iSectionDetailsRepository.findAllById(anyIterable())).thenReturn(List.of(sectionDetails));
    when(sectionVisibilityRepository.save(sectionVisibility)).thenReturn(sectionVisibility);
    when(modelMapper.map(sectionVisibility, SectionVisibilityResponse.class)).thenReturn(
        buildResult);
    UsersDto usersDto = new UsersDto();
    usersDto.setTenantId(1);
    UserContext.setUser(usersDto);
    sectionVisibilityService.update(sectionVisibilityRequest);
    verify(sectionVisibilityRepository).save(any());
    verify(modelMapper, times(2)).map(any(), any());
    verify(sectionVisibilityRepository).findById(anyLong());
  }
  @Test
  void testGetAllByBranchCode() {
    SectionVisibility sectionVisibility = new SectionVisibility();
    sectionVisibility.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
    sectionVisibility.setBranchCode("Test");
    sectionVisibility.setMode("SEA");
    sectionVisibility.setDirection("IMP");
    sectionVisibility.setGuid(UUID.randomUUID());
    sectionVisibility.setId(1L);
    sectionVisibility.setIsDeleted(true);
    sectionVisibility.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
    sectionVisibility.setUpdatedBy("2020-03-01");
    SectionVisibilityResponse buildResult = SectionVisibilityResponse.builder()
        .branch("Test")
        .mode("SEA")
        .direction("IMP")
        .id(1L)
        .build();
    when(modelMapper.map(Mockito.<Object>any(),
        Mockito.<Class<SectionVisibilityResponse>>any())).thenReturn(buildResult);
    when(sectionVisibilityRepository.findByBranchCode("Test")).thenReturn(List.of(sectionVisibility));
    sectionVisibilityService.getByBranchCode("Test");
    verify(sectionVisibilityRepository).findByBranchCode("Test");
    verify(modelMapper).map(Mockito.<Object>any(), Mockito.<Class<SectionDetailsResponse>>any());
  }
}
