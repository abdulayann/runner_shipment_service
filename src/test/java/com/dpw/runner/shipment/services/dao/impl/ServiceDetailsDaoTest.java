package com.dpw.runner.shipment.services.dao.impl;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ServiceDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IServiceDetailsRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ContextConfiguration(classes = {ServiceDetailsDao.class})
@ExtendWith(SpringExtension.class)
class ServiceDetailsDaoTest {
    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;
    private static ShipmentDetails completeShipment;
    @MockBean
    private IAuditLogService iAuditLogService;

    @MockBean
    private IServiceDetailsRepository iServiceDetailsRepository;

    @MockBean
    private JsonHelper jsonHelper;

    @Autowired
    private ServiceDetailsDao serviceDetailsDao;

    @MockBean
    private ValidatorUtility validatorUtility;

    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
    }

    @BeforeEach
    void setup() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(V1TenantSettingsResponse.builder().P100Branch(false).build());
        completeShipment = jsonTestUtility.getCompleteShipment();
    }

    /**
     * Method under test: {@link ServiceDetailsDao#save(ServiceDetails)}
     */
    @Test
    void testSave() {
        // Arrange
        when(jsonHelper.convertToJson(Mockito.<ServiceDetails>any())).thenReturn("Convert To Json");

        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");
        when(iServiceDetailsRepository.save(Mockito.<ServiceDetails>any())).thenReturn(serviceDetails);
        when(validatorUtility.applyValidation(Mockito.<String>any(), Mockito.<String>any(), Mockito.<LifecycleHooks>any(),
                anyBoolean())).thenReturn(new HashSet<>());

        Parties contractor2 = new Parties();
        contractor2.setAddressCode("42 Main St");
        contractor2.setAddressData(new HashMap<>());
        contractor2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor2.setEntityId(1L);
        contractor2.setEntityType("Entity Type");
        contractor2.setGuid(UUID.randomUUID());
        contractor2.setId(1L);
        contractor2.setIsAddressFreeText(true);
        contractor2.setIsDeleted(true);
        contractor2.setOrgCode("Org Code");
        contractor2.setOrgData(new HashMap<>());
        contractor2.setTenantId(1);
        contractor2.setType("Type");
        contractor2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails2 = new ServiceDetails();
        serviceDetails2.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setConsolidationId(1L);
        serviceDetails2.setContractor(contractor2);
        serviceDetails2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails2.setGuid(UUID.randomUUID());
        serviceDetails2.setId(1L);
        serviceDetails2.setIsDeleted(true);
        serviceDetails2.setRefNumber("42");
        serviceDetails2.setServiceCount(3L);
        serviceDetails2.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails2.setServiceNotes("Service Notes");
        serviceDetails2.setServiceType("Service Type");
        serviceDetails2.setShipmentId(1L);
        serviceDetails2.setSrvLocation("Srv Location");
        serviceDetails2.setTenantId(1);
        serviceDetails2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setUpdatedBy("2020-03-01");

        // Act
        ServiceDetails actualSaveResult = serviceDetailsDao.save(serviceDetails2);

        // Assert
        verify(jsonHelper).convertToJson(isA(ServiceDetails.class));
        verify(validatorUtility).applyValidation(anyString(),anyString(), any(),
                anyBoolean());
        verify(iServiceDetailsRepository).save(isA(ServiceDetails.class));
        assertSame(serviceDetails, actualSaveResult);
    }

    /**
     * Method under test: {@link ServiceDetailsDao#save(ServiceDetails)}
     */
    @Test
    void testSave2() {
        // Arrange
        when(jsonHelper.convertToJson(Mockito.<ServiceDetails>any())).thenReturn("Convert To Json");
        when(validatorUtility.applyValidation(Mockito.<String>any(), Mockito.<String>any(), Mockito.<LifecycleHooks>any(),
                anyBoolean())).thenThrow(new ValidationException("SERVICE_DETAILS"));

        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");

        // Act and Assert
        assertThrows(ValidationException.class, () -> serviceDetailsDao.save(serviceDetails));
        verify(jsonHelper).convertToJson(isA(ServiceDetails.class));
        verify(validatorUtility).applyValidation(anyString(),anyString(), any(),
                anyBoolean());
    }

    /**
     * Method under test: {@link ServiceDetailsDao#saveAll(List)}
     */
    @Test
    void testSaveAll() {
        // Arrange
        ArrayList<ServiceDetails> serviceDetailsList = new ArrayList<>();
        when(iServiceDetailsRepository.saveAll(Mockito.<Iterable<ServiceDetails>>any())).thenReturn(serviceDetailsList);

        // Act
        List<ServiceDetails> actualSaveAllResult = serviceDetailsDao.saveAll(new ArrayList<>());

        // Assert
        verify(iServiceDetailsRepository).saveAll(isA(Iterable.class));
        assertTrue(actualSaveAllResult.isEmpty());
        assertSame(serviceDetailsList, actualSaveAllResult);
    }

    /**
     * Method under test: {@link ServiceDetailsDao#saveAll(List)}
     */
    @Test
    void testSaveAll2() {
        // Arrange
        when(jsonHelper.convertToJson(Mockito.<ServiceDetails>any())).thenReturn("Convert To Json");
        ArrayList<ServiceDetails> serviceDetailsList = new ArrayList<>();
        when(iServiceDetailsRepository.saveAll(Mockito.<Iterable<ServiceDetails>>any())).thenReturn(serviceDetailsList);
        when(validatorUtility.applyValidation(Mockito.<String>any(), Mockito.<String>any(), Mockito.<LifecycleHooks>any(),
                anyBoolean())).thenReturn(new HashSet<>());

        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");

        ArrayList<ServiceDetails> serviceDetailsList2 = new ArrayList<>();
        serviceDetailsList2.add(serviceDetails);

        // Act
        List<ServiceDetails> actualSaveAllResult = serviceDetailsDao.saveAll(serviceDetailsList2);

        // Assert
        verify(jsonHelper).convertToJson(isA(ServiceDetails.class));
        verify(validatorUtility).applyValidation(anyString(),anyString(), any(),
                anyBoolean());
        verify(iServiceDetailsRepository).saveAll(isA(Iterable.class));
        assertTrue(actualSaveAllResult.isEmpty());
        assertSame(serviceDetailsList, actualSaveAllResult);
    }

    /**
     * Method under test: {@link ServiceDetailsDao#saveAll(List)}
     */
    @Test
    void testSaveAll3() {
        // Arrange
        when(jsonHelper.convertToJson(Mockito.<ServiceDetails>any())).thenReturn("Convert To Json");
        when(validatorUtility.applyValidation(Mockito.<String>any(), Mockito.<String>any(), Mockito.<LifecycleHooks>any(),
                anyBoolean())).thenThrow(new ValidationException("SERVICE_DETAILS"));

        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");

        ArrayList<ServiceDetails> serviceDetailsList = new ArrayList<>();
        serviceDetailsList.add(serviceDetails);

        // Act and Assert
        assertThrows(ValidationException.class, () -> serviceDetailsDao.saveAll(serviceDetailsList));
        verify(jsonHelper).convertToJson(isA(ServiceDetails.class));
        verify(validatorUtility).applyValidation(anyString(),anyString(), any(), anyBoolean());
    }

    /**
     * Method under test: {@link ServiceDetailsDao#saveAll(List)}
     */
    @Test
    void testSaveAll4() {
        // Arrange
        when(jsonHelper.convertToJson(Mockito.<ServiceDetails>any())).thenReturn("Convert To Json");

        HashSet<String> stringSet = new HashSet<>();
        stringSet.add("SERVICE_DETAILS");
        when(validatorUtility.applyValidation(Mockito.<String>any(), Mockito.<String>any(), Mockito.<LifecycleHooks>any(),
                anyBoolean())).thenReturn(stringSet);

        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");

        ArrayList<ServiceDetails> serviceDetailsList = new ArrayList<>();
        serviceDetailsList.add(serviceDetails);

        // Act and Assert
        assertThrows(ValidationException.class, () -> serviceDetailsDao.saveAll(serviceDetailsList));
        verify(jsonHelper).convertToJson(isA(ServiceDetails.class));
        verify(validatorUtility).applyValidation(anyString(),anyString(), any(), anyBoolean());
    }

    /**
     * Method under test: {@link ServiceDetailsDao#saveAll(List)}
     */
    @Test
    void testSaveAll5() {
        // Arrange
        when(jsonHelper.convertToJson(Mockito.<ServiceDetails>any())).thenReturn("Convert To Json");
        ArrayList<ServiceDetails> serviceDetailsList = new ArrayList<>();
        when(iServiceDetailsRepository.saveAll(Mockito.<Iterable<ServiceDetails>>any())).thenReturn(serviceDetailsList);
        when(validatorUtility.applyValidation(Mockito.<String>any(), Mockito.<String>any(), Mockito.<LifecycleHooks>any(),
                anyBoolean())).thenReturn(new HashSet<>());

        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");

        Parties contractor2 = new Parties();
        contractor2.setAddressCode("17 High St");
        contractor2.setAddressData(new HashMap<>());
        contractor2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setCreatedBy("SERVICE_DETAILS");
        contractor2.setEntityId(2L);
        contractor2.setEntityType("Entity Type");
        contractor2.setGuid(UUID.randomUUID());
        contractor2.setId(2L);
        contractor2.setIsAddressFreeText(false);
        contractor2.setIsDeleted(false);
        contractor2.setOrgCode("Org Code");
        contractor2.setOrgData(new HashMap<>());
        contractor2.setTenantId(2);
        contractor2.setType("Type");
        contractor2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setUpdatedBy("2020/03/01");

        ServiceDetails serviceDetails2 = new ServiceDetails();
        serviceDetails2.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setConsolidationId(2L);
        serviceDetails2.setContractor(contractor2);
        serviceDetails2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCreatedBy("SERVICE_DETAILS");
        serviceDetails2.setGuid(UUID.randomUUID());
        serviceDetails2.setId(2L);
        serviceDetails2.setIsDeleted(false);
        serviceDetails2.setRefNumber("SERVICE_DETAILS");
        serviceDetails2.setServiceCount(1L);
        serviceDetails2.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails2.setServiceNotes("Service Notes");
        serviceDetails2.setServiceType("Service Type");
        serviceDetails2.setShipmentId(2L);
        serviceDetails2.setSrvLocation("Srv Location");
        serviceDetails2.setTenantId(2);
        serviceDetails2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setUpdatedBy("2020/03/01");

        ArrayList<ServiceDetails> serviceDetailsList2 = new ArrayList<>();
        serviceDetailsList2.add(serviceDetails2);
        serviceDetailsList2.add(serviceDetails);

        // Act
        List<ServiceDetails> actualSaveAllResult = serviceDetailsDao.saveAll(serviceDetailsList2);

        // Assert
        verify(jsonHelper, atLeast(1)).convertToJson(Mockito.<ServiceDetails>any());
        verify(validatorUtility, atLeast(1)).applyValidation(anyString(),anyString(), any(),
                anyBoolean());
        verify(iServiceDetailsRepository).saveAll(isA(Iterable.class));
        assertTrue(actualSaveAllResult.isEmpty());
        assertSame(serviceDetailsList, actualSaveAllResult);
    }

    /**
     * Method under test: {@link ServiceDetailsDao#findAll(Specification, Pageable)}
     */
    @Test
    void testFindAll() {
        // Arrange
        PageImpl<ServiceDetails> pageImpl = new PageImpl<>(new ArrayList<>());
        when(iServiceDetailsRepository.findAll(Mockito.<Specification<ServiceDetails>>any(), Mockito.<Pageable>any()))
                .thenReturn(pageImpl);

        // Act
        Page<ServiceDetails> actualFindAllResult = serviceDetailsDao.findAll(null, null);

        // Assert
        verify(iServiceDetailsRepository).findAll((Specification<ServiceDetails>) isNull(), (Pageable) isNull());
        assertTrue(actualFindAllResult.toList().isEmpty());
        assertSame(pageImpl, actualFindAllResult);
    }

    /**
     * Method under test: {@link ServiceDetailsDao#findAll(Specification, Pageable)}
     */
    @Test
    void testFindAll2() {
        // Arrange
        when(iServiceDetailsRepository.findAll(Mockito.<Specification<ServiceDetails>>any(), Mockito.<Pageable>any()))
                .thenThrow(new ValidationException("Msg"));

        // Act and Assert
        assertThrows(ValidationException.class, () -> serviceDetailsDao.findAll(null, null));
        verify(iServiceDetailsRepository).findAll((Specification<ServiceDetails>) isNull(), (Pageable) isNull());
    }

    /**
     * Method under test: {@link ServiceDetailsDao#findById(Long)}
     */
    @Test
    void testFindById() {
        // Arrange
        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");
        Optional<ServiceDetails> ofResult = Optional.of(serviceDetails);
        when(iServiceDetailsRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);

        // Act
        Optional<ServiceDetails> actualFindByIdResult = serviceDetailsDao.findById(1L);

        // Assert
        verify(iServiceDetailsRepository).findById(any());
        assertSame(ofResult, actualFindByIdResult);
    }

    /**
     * Method under test: {@link ServiceDetailsDao#findById(Long)}
     */
    @Test
    void testFindById2() {
        // Arrange
        when(iServiceDetailsRepository.findById(Mockito.<Long>any())).thenThrow(new ValidationException("Msg"));

        // Act and Assert
        assertThrows(ValidationException.class, () -> serviceDetailsDao.findById(1L));
        verify(iServiceDetailsRepository).findById(any());
    }

    /**
     * Method under test: {@link ServiceDetailsDao#delete(ServiceDetails)}
     */
    @Test
    void testDelete() {
        // Arrange
        doNothing().when(iServiceDetailsRepository).delete(Mockito.<ServiceDetails>any());

        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");

        // Act
        serviceDetailsDao.delete(serviceDetails);

        // Assert that nothing has changed
        verify(iServiceDetailsRepository).delete(isA(ServiceDetails.class));
        LocalDateTime bookingDate = serviceDetails.getBookingDate();
        assertEquals("1970-01-01", bookingDate.toLocalDate().toString());
        assertEquals("1970-01-01", serviceDetails.getCompletionDate().toLocalDate().toString());
        assertEquals("1970-01-01", serviceDetails.getCreatedAt().toLocalDate().toString());
        assertEquals("1970-01-01", serviceDetails.getUpdatedAt().toLocalDate().toString());
        assertEquals("2020-03-01", serviceDetails.getUpdatedBy());
        assertEquals("42 Main St", serviceDetails.getContractor().getAddressCode());
        assertEquals("42", serviceDetails.getRefNumber());
        assertEquals("Jan 1, 2020 8:00am GMT+0100", serviceDetails.getCreatedBy());
        assertEquals("Service Notes", serviceDetails.getServiceNotes());
        assertEquals("Service Type", serviceDetails.getServiceType());
        assertEquals("Srv Location", serviceDetails.getSrvLocation());
        assertEquals(1, serviceDetails.getTenantId().intValue());
        assertEquals(1L, serviceDetails.getConsolidationId().longValue());
        assertEquals(1L, serviceDetails.getShipmentId().longValue());
        assertEquals(1L, serviceDetails.getId().longValue());
        assertEquals(2, serviceDetails.getGuid().variant());
        assertEquals(3L, serviceDetails.getServiceCount().longValue());
        assertTrue(serviceDetails.getIsDeleted());
        LocalTime expectedServiceDuration = bookingDate.toLocalTime();
        assertSame(expectedServiceDuration, serviceDetails.getServiceDuration());
    }

    /**
     * Method under test: {@link ServiceDetailsDao#delete(ServiceDetails)}
     */
    @Test
    void testDelete2() {
        // Arrange
        doThrow(new ValidationException("Msg")).when(iServiceDetailsRepository).delete(Mockito.<ServiceDetails>any());

        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");

        // Act and Assert
        assertThrows(ValidationException.class, () -> serviceDetailsDao.delete(serviceDetails));
        verify(iServiceDetailsRepository).delete(isA(ServiceDetails.class));
    }

    /**
     * Method under test:
     * {@link ServiceDetailsDao#updateEntityFromShipment(List, Long)}
     */
    @Test
    void testUpdateEntityFromShipment() throws RunnerException {
        // Arrange
        when(iServiceDetailsRepository.findAll(Mockito.<Specification<ServiceDetails>>any(), Mockito.<Pageable>any()))
                .thenReturn(new PageImpl<>(new ArrayList<>()));

        // Act
        List<ServiceDetails> actualUpdateEntityFromShipmentResult = serviceDetailsDao
                .updateEntityFromShipment(new ArrayList<>(), 1L);

        // Assert
        assertTrue(actualUpdateEntityFromShipmentResult.isEmpty());
    }

    /**
     * Method under test:
     * {@link ServiceDetailsDao#updateEntityFromShipment(List, Long)}
     */
    @Test
    void testUpdateEntityFromShipment2() throws RunnerException, JsonProcessingException, IllegalAccessException,
            NoSuchFieldException, NoSuchMethodException, InvocationTargetException {
        // Arrange
        doNothing().when(iAuditLogService).addAuditLog(Mockito.<AuditLogMetaData>any());

        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<ServiceDetails>>any()))
                .thenReturn(serviceDetails);
        when(jsonHelper.convertToJson(Mockito.<ServiceDetails>any())).thenReturn("Convert To Json");

        Parties contractor2 = new Parties();
        contractor2.setAddressCode("42 Main St");
        contractor2.setAddressData(new HashMap<>());
        contractor2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor2.setEntityId(1L);
        contractor2.setEntityType("shipmentId");
        contractor2.setGuid(UUID.randomUUID());
        contractor2.setId(1L);
        contractor2.setIsAddressFreeText(true);
        contractor2.setIsDeleted(true);
        contractor2.setOrgCode("shipmentId");
        contractor2.setOrgData(new HashMap<>());
        contractor2.setTenantId(1);
        contractor2.setType("shipmentId");
        contractor2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails2 = new ServiceDetails();
        serviceDetails2.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setConsolidationId(1L);
        serviceDetails2.setContractor(contractor2);
        serviceDetails2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails2.setGuid(UUID.randomUUID());
        serviceDetails2.setId(1L);
        serviceDetails2.setIsDeleted(true);
        serviceDetails2.setRefNumber("42");
        serviceDetails2.setServiceCount(3L);
        serviceDetails2.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails2.setServiceNotes("shipmentId");
        serviceDetails2.setServiceType("shipmentId");
        serviceDetails2.setShipmentId(1L);
        serviceDetails2.setSrvLocation("shipmentId");
        serviceDetails2.setTenantId(1);
        serviceDetails2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setUpdatedBy("2020-03-01");

        ArrayList<ServiceDetails> content = new ArrayList<>();
        content.add(serviceDetails2);
        doNothing().when(iServiceDetailsRepository).delete(Mockito.<ServiceDetails>any());
        when(iServiceDetailsRepository.findByShipmentId(any()))
                .thenReturn(content);

        // Act
        List<ServiceDetails> actualUpdateEntityFromShipmentResult = serviceDetailsDao
                .updateEntityFromShipment(new ArrayList<>(), 1L);

        // Assert
        verify(jsonHelper).convertToJson(isA(ServiceDetails.class));
        verify(jsonHelper).readFromJson(any(), isA(Class.class));
        verify(iServiceDetailsRepository).findByShipmentId(isA(Long.class));
        verify(iAuditLogService).addAuditLog(isA(AuditLogMetaData.class));
        verify(iServiceDetailsRepository).delete(isA(ServiceDetails.class));
        assertTrue(actualUpdateEntityFromShipmentResult.isEmpty());
    }

    /**
     * Method under test:
     * {@link ServiceDetailsDao#updateEntityFromShipment(List, Long)}
     */
    @Test
    void testUpdateEntityFromShipment3() throws RunnerException {
        // Arrange
        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");
        Class<Object> forNameResult = Object.class;
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<Object>>any())).thenReturn(forNameResult);
        when(jsonHelper.convertToJson(Mockito.<ServiceDetails>any())).thenReturn("Convert To Json");

        Parties contractor2 = new Parties();
        contractor2.setAddressCode("42 Main St");
        contractor2.setAddressData(new HashMap<>());
        contractor2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor2.setEntityId(1L);
        contractor2.setEntityType("shipmentId");
        contractor2.setGuid(UUID.randomUUID());
        contractor2.setId(1L);
        contractor2.setIsAddressFreeText(true);
        contractor2.setIsDeleted(true);
        contractor2.setOrgCode("shipmentId");
        contractor2.setOrgData(new HashMap<>());
        contractor2.setTenantId(1);
        contractor2.setType("shipmentId");
        contractor2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails2 = new ServiceDetails();
        serviceDetails2.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setConsolidationId(1L);
        serviceDetails2.setContractor(contractor2);
        serviceDetails2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails2.setGuid(UUID.randomUUID());
        serviceDetails2.setId(1L);
        serviceDetails2.setIsDeleted(true);
        serviceDetails2.setRefNumber("42");
        serviceDetails2.setServiceCount(3L);
        serviceDetails2.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails2.setServiceNotes("shipmentId");
        serviceDetails2.setServiceType("shipmentId");
        serviceDetails2.setShipmentId(1L);
        serviceDetails2.setSrvLocation("shipmentId");
        serviceDetails2.setTenantId(1);
        serviceDetails2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setUpdatedBy("2020-03-01");

        ArrayList<ServiceDetails> content = new ArrayList<>();
        content.add(serviceDetails2);
        doThrow(new ValidationException("shipmentId")).when(iServiceDetailsRepository)
                .delete(Mockito.<ServiceDetails>any());
        when(iServiceDetailsRepository.findByShipmentId(any()))
                .thenReturn(content);

        // Act
        List<ServiceDetails> actualUpdateEntityFromShipmentResult = serviceDetailsDao
                .updateEntityFromShipment(new ArrayList<>(), 1L);

        // Assert
        verify(jsonHelper).convertToJson(isA(ServiceDetails.class));
        verify(iServiceDetailsRepository).findByShipmentId(isA(Long.class));
        verify(iServiceDetailsRepository).delete(isA(ServiceDetails.class));
        assertTrue(actualUpdateEntityFromShipmentResult.isEmpty());
    }

    /**
     * Method under test:
     * {@link ServiceDetailsDao#updateEntityFromShipment(List, Long, List)}
     */
    @Test
    void testUpdateEntityFromShipment4() throws RunnerException {
        // Arrange
        ArrayList<ServiceDetails> serviceDetailsList = new ArrayList<>();

        // Act and Assert
        assertTrue(serviceDetailsDao.updateEntityFromShipment(serviceDetailsList, 1L, new ArrayList<>()).isEmpty());
    }

    /**
     * Method under test:
     * {@link ServiceDetailsDao#updateEntityFromShipment(List, Long, List)}
     */
    @Test
    void testUpdateEntityFromShipment5() throws RunnerException, JsonProcessingException, IllegalAccessException,
            NoSuchFieldException, NoSuchMethodException, InvocationTargetException {
        // Arrange
        doNothing().when(iAuditLogService).addAuditLog(Mockito.<AuditLogMetaData>any());

        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<ServiceDetails>>any()))
                .thenReturn(serviceDetails);
        when(jsonHelper.convertToJson(Mockito.<ServiceDetails>any())).thenReturn("Convert To Json");

        Parties contractor2 = new Parties();
        contractor2.setAddressCode("42 Main St");
        contractor2.setAddressData(new HashMap<>());
        contractor2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor2.setEntityId(1L);
        contractor2.setEntityType("Entity Type");
        contractor2.setGuid(UUID.randomUUID());
        contractor2.setId(1L);
        contractor2.setIsAddressFreeText(true);
        contractor2.setIsDeleted(true);
        contractor2.setOrgCode("Org Code");
        contractor2.setOrgData(new HashMap<>());
        contractor2.setTenantId(1);
        contractor2.setType("Type");
        contractor2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails2 = new ServiceDetails();
        serviceDetails2.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setConsolidationId(1L);
        serviceDetails2.setContractor(contractor2);
        serviceDetails2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails2.setGuid(UUID.randomUUID());
        serviceDetails2.setId(1L);
        serviceDetails2.setIsDeleted(true);
        serviceDetails2.setRefNumber("42");
        serviceDetails2.setServiceCount(3L);
        serviceDetails2.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails2.setServiceNotes("Service Notes");
        serviceDetails2.setServiceType("Service Type");
        serviceDetails2.setShipmentId(1L);
        serviceDetails2.setSrvLocation("Srv Location");
        serviceDetails2.setTenantId(1);
        serviceDetails2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setUpdatedBy("2020-03-01");
        Optional<ServiceDetails> ofResult = Optional.of(serviceDetails2);

        Parties contractor3 = new Parties();
        contractor3.setAddressCode("42 Main St");
        contractor3.setAddressData(new HashMap<>());
        contractor3.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor3.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor3.setEntityId(1L);
        contractor3.setEntityType("Entity Type");
        contractor3.setGuid(UUID.randomUUID());
        contractor3.setId(1L);
        contractor3.setIsAddressFreeText(true);
        contractor3.setIsDeleted(true);
        contractor3.setOrgCode("Org Code");
        contractor3.setOrgData(new HashMap<>());
        contractor3.setTenantId(1);
        contractor3.setType("Type");
        contractor3.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor3.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails3 = new ServiceDetails();
        serviceDetails3.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setConsolidationId(1L);
        serviceDetails3.setContractor(contractor3);
        serviceDetails3.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails3.setGuid(UUID.randomUUID());
        serviceDetails3.setId(1L);
        serviceDetails3.setIsDeleted(true);
        serviceDetails3.setRefNumber("42");
        serviceDetails3.setServiceCount(3L);
        serviceDetails3.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails3.setServiceNotes("Service Notes");
        serviceDetails3.setServiceType("Service Type");
        serviceDetails3.setShipmentId(1L);
        serviceDetails3.setSrvLocation("Srv Location");
        serviceDetails3.setTenantId(1);
        serviceDetails3.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setUpdatedBy("2020-03-01");
        when(iServiceDetailsRepository.save(Mockito.<ServiceDetails>any())).thenReturn(serviceDetails3);
        when(iServiceDetailsRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        when(validatorUtility.applyValidation(Mockito.<String>any(), Mockito.<String>any(), Mockito.<LifecycleHooks>any(),
                anyBoolean())).thenReturn(new HashSet<>());

        Parties contractor4 = new Parties();
        contractor4.setAddressCode("42 Main St");
        contractor4.setAddressData(new HashMap<>());
        contractor4.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor4.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor4.setEntityId(1L);
        contractor4.setEntityType("Entity Type");
        contractor4.setGuid(UUID.randomUUID());
        contractor4.setId(1L);
        contractor4.setIsAddressFreeText(true);
        contractor4.setIsDeleted(true);
        contractor4.setOrgCode("Org Code");
        contractor4.setOrgData(new HashMap<>());
        contractor4.setTenantId(1);
        contractor4.setType("Type");
        contractor4.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor4.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails4 = new ServiceDetails();
        serviceDetails4.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails4.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails4.setConsolidationId(1L);
        serviceDetails4.setContractor(contractor4);
        serviceDetails4.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails4.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails4.setGuid(UUID.randomUUID());
        serviceDetails4.setId(1L);
        serviceDetails4.setIsDeleted(true);
        serviceDetails4.setRefNumber("42");
        serviceDetails4.setServiceCount(3L);
        serviceDetails4.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails4.setServiceNotes("Service Notes");
        serviceDetails4.setServiceType("Service Type");
        serviceDetails4.setShipmentId(1L);
        serviceDetails4.setSrvLocation("Srv Location");
        serviceDetails4.setTenantId(1);
        serviceDetails4.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails4.setUpdatedBy("2020-03-01");

        ArrayList<ServiceDetails> serviceDetailsList = new ArrayList<>();
        serviceDetailsList.add(serviceDetails4);

        // Act
        List<ServiceDetails> actualUpdateEntityFromShipmentResult = serviceDetailsDao
                .updateEntityFromShipment(serviceDetailsList, 1L, new ArrayList<>());

        // Assert
        verify(jsonHelper, atLeast(1)).convertToJson(isA(ServiceDetails.class));
        verify(jsonHelper).readFromJson(any(), isA(Class.class));
        verify(iServiceDetailsRepository).findById(any());
        verify(iAuditLogService).addAuditLog(isA(AuditLogMetaData.class));
        verify(validatorUtility).applyValidation(anyString(),anyString(), any(),
                anyBoolean());
        verify(iServiceDetailsRepository).save(isA(ServiceDetails.class));
        assertEquals(1, actualUpdateEntityFromShipmentResult.size());
    }

    /**
     * Method under test:
     * {@link ServiceDetailsDao#updateEntityFromShipment(List, Long, List)}
     */
    @Test
    void testUpdateEntityFromShipment6() throws RunnerException {
        // Arrange
        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");
        Class<Object> forNameResult = Object.class;
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<Object>>any())).thenReturn(forNameResult);

        when(jsonHelper.convertToJson(Mockito.<ServiceDetails>any())).thenReturn("Convert To Json");

        Parties contractor2 = new Parties();
        contractor2.setAddressCode("42 Main St");
        contractor2.setAddressData(new HashMap<>());
        contractor2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor2.setEntityId(1L);
        contractor2.setEntityType("Entity Type");
        contractor2.setGuid(UUID.randomUUID());
        contractor2.setId(1L);
        contractor2.setIsAddressFreeText(true);
        contractor2.setIsDeleted(true);
        contractor2.setOrgCode("Org Code");
        contractor2.setOrgData(new HashMap<>());
        contractor2.setTenantId(1);
        contractor2.setType("Type");
        contractor2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails2 = new ServiceDetails();
        serviceDetails2.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setConsolidationId(1L);
        serviceDetails2.setContractor(contractor2);
        serviceDetails2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails2.setGuid(UUID.randomUUID());
        serviceDetails2.setId(1L);
        serviceDetails2.setIsDeleted(true);
        serviceDetails2.setRefNumber("42");
        serviceDetails2.setServiceCount(3L);
        serviceDetails2.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails2.setServiceNotes("Service Notes");
        serviceDetails2.setServiceType("Service Type");
        serviceDetails2.setShipmentId(1L);
        serviceDetails2.setSrvLocation("Srv Location");
        serviceDetails2.setTenantId(1);
        serviceDetails2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setUpdatedBy("2020-03-01");
        Optional<ServiceDetails> ofResult = Optional.of(serviceDetails2);
        when(iServiceDetailsRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        when(validatorUtility.applyValidation(Mockito.<String>any(), Mockito.<String>any(), Mockito.<LifecycleHooks>any(),
                anyBoolean())).thenThrow(new ValidationException("SERVICE_DETAILS"));

        Parties contractor3 = new Parties();
        contractor3.setAddressCode("42 Main St");
        contractor3.setAddressData(new HashMap<>());
        contractor3.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor3.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor3.setEntityId(1L);
        contractor3.setEntityType("Entity Type");
        contractor3.setGuid(UUID.randomUUID());
        contractor3.setId(1L);
        contractor3.setIsAddressFreeText(true);
        contractor3.setIsDeleted(true);
        contractor3.setOrgCode("Org Code");
        contractor3.setOrgData(new HashMap<>());
        contractor3.setTenantId(1);
        contractor3.setType("Type");
        contractor3.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor3.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails3 = new ServiceDetails();
        serviceDetails3.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setConsolidationId(1L);
        serviceDetails3.setContractor(contractor3);
        serviceDetails3.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails3.setGuid(UUID.randomUUID());
        serviceDetails3.setId(1L);
        serviceDetails3.setIsDeleted(true);
        serviceDetails3.setRefNumber("42");
        serviceDetails3.setServiceCount(3L);
        serviceDetails3.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails3.setServiceNotes("Service Notes");
        serviceDetails3.setServiceType("Service Type");
        serviceDetails3.setShipmentId(1L);
        serviceDetails3.setSrvLocation("Srv Location");
        serviceDetails3.setTenantId(1);
        serviceDetails3.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setUpdatedBy("2020-03-01");

        ArrayList<ServiceDetails> serviceDetailsList = new ArrayList<>();
        serviceDetailsList.add(serviceDetails3);

        // Act and Assert
        assertThrows(RunnerException.class,
                () -> serviceDetailsDao.updateEntityFromShipment(serviceDetailsList, 1L, new ArrayList<>()));
        verify(jsonHelper, atLeast(1)).convertToJson(isA(ServiceDetails.class));
        verify(iServiceDetailsRepository).findById(any());
        verify(validatorUtility).applyValidation(anyString(),anyString(), any(),
                anyBoolean());
    }

    /**
     * Method under test:
     * {@link ServiceDetailsDao#saveEntityFromShipment(List, Long)}
     */
    @Test
    void testSaveEntityFromShipment() {
        // Arrange, Act and Assert
        assertTrue(serviceDetailsDao.saveEntityFromShipment(new ArrayList<>(), 1L).isEmpty());
    }

    /**
     * Method under test:
     * {@link ServiceDetailsDao#saveEntityFromShipment(List, Long)}
     */
    @Test
    void testSaveEntityFromShipment2() throws RunnerException, JsonProcessingException, IllegalAccessException,
            NoSuchFieldException, NoSuchMethodException, InvocationTargetException {
        // Arrange
        doNothing().when(iAuditLogService).addAuditLog(Mockito.<AuditLogMetaData>any());

        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<ServiceDetails>>any()))
                .thenReturn(serviceDetails);
        when(jsonHelper.convertToJson(Mockito.<ServiceDetails>any())).thenReturn("Convert To Json");

        Parties contractor2 = new Parties();
        contractor2.setAddressCode("42 Main St");
        contractor2.setAddressData(new HashMap<>());
        contractor2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor2.setEntityId(1L);
        contractor2.setEntityType("Entity Type");
        contractor2.setGuid(UUID.randomUUID());
        contractor2.setId(1L);
        contractor2.setIsAddressFreeText(true);
        contractor2.setIsDeleted(true);
        contractor2.setOrgCode("Org Code");
        contractor2.setOrgData(new HashMap<>());
        contractor2.setTenantId(1);
        contractor2.setType("Type");
        contractor2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails2 = new ServiceDetails();
        serviceDetails2.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setConsolidationId(1L);
        serviceDetails2.setContractor(contractor2);
        serviceDetails2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails2.setGuid(UUID.randomUUID());
        serviceDetails2.setId(1L);
        serviceDetails2.setIsDeleted(true);
        serviceDetails2.setRefNumber("42");
        serviceDetails2.setServiceCount(3L);
        serviceDetails2.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails2.setServiceNotes("Service Notes");
        serviceDetails2.setServiceType("Service Type");
        serviceDetails2.setShipmentId(1L);
        serviceDetails2.setSrvLocation("Srv Location");
        serviceDetails2.setTenantId(1);
        serviceDetails2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setUpdatedBy("2020-03-01");
        Optional<ServiceDetails> ofResult = Optional.of(serviceDetails2);

        Parties contractor3 = new Parties();
        contractor3.setAddressCode("42 Main St");
        contractor3.setAddressData(new HashMap<>());
        contractor3.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor3.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor3.setEntityId(1L);
        contractor3.setEntityType("Entity Type");
        contractor3.setGuid(UUID.randomUUID());
        contractor3.setId(1L);
        contractor3.setIsAddressFreeText(true);
        contractor3.setIsDeleted(true);
        contractor3.setOrgCode("Org Code");
        contractor3.setOrgData(new HashMap<>());
        contractor3.setTenantId(1);
        contractor3.setType("Type");
        contractor3.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor3.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails3 = new ServiceDetails();
        serviceDetails3.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setConsolidationId(1L);
        serviceDetails3.setContractor(contractor3);
        serviceDetails3.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails3.setGuid(UUID.randomUUID());
        serviceDetails3.setId(1L);
        serviceDetails3.setIsDeleted(true);
        serviceDetails3.setRefNumber("42");
        serviceDetails3.setServiceCount(3L);
        serviceDetails3.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails3.setServiceNotes("Service Notes");
        serviceDetails3.setServiceType("Service Type");
        serviceDetails3.setShipmentId(1L);
        serviceDetails3.setSrvLocation("Srv Location");
        serviceDetails3.setTenantId(1);
        serviceDetails3.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setUpdatedBy("2020-03-01");
        when(iServiceDetailsRepository.save(Mockito.<ServiceDetails>any())).thenReturn(serviceDetails3);
        when(iServiceDetailsRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        when(validatorUtility.applyValidation(Mockito.<String>any(), Mockito.<String>any(), Mockito.<LifecycleHooks>any(),
                anyBoolean())).thenReturn(new HashSet<>());

        Parties contractor4 = new Parties();
        contractor4.setAddressCode("42 Main St");
        contractor4.setAddressData(new HashMap<>());
        contractor4.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor4.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor4.setEntityId(1L);
        contractor4.setEntityType("Entity Type");
        contractor4.setGuid(UUID.randomUUID());
        contractor4.setId(1L);
        contractor4.setIsAddressFreeText(true);
        contractor4.setIsDeleted(true);
        contractor4.setOrgCode("Org Code");
        contractor4.setOrgData(new HashMap<>());
        contractor4.setTenantId(1);
        contractor4.setType("Type");
        contractor4.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor4.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails4 = new ServiceDetails();
        serviceDetails4.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails4.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails4.setConsolidationId(1L);
        serviceDetails4.setContractor(contractor4);
        serviceDetails4.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails4.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails4.setGuid(UUID.randomUUID());
        serviceDetails4.setId(1L);
        serviceDetails4.setIsDeleted(true);
        serviceDetails4.setRefNumber("42");
        serviceDetails4.setServiceCount(3L);
        serviceDetails4.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails4.setServiceNotes("Service Notes");
        serviceDetails4.setServiceType("Service Type");
        serviceDetails4.setShipmentId(1L);
        serviceDetails4.setSrvLocation("Srv Location");
        serviceDetails4.setTenantId(1);
        serviceDetails4.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails4.setUpdatedBy("2020-03-01");

        ArrayList<ServiceDetails> serviceDetailsRequests = new ArrayList<>();
        serviceDetailsRequests.add(serviceDetails4);

        // Act
        List<ServiceDetails> actualSaveEntityFromShipmentResult = serviceDetailsDao
                .saveEntityFromShipment(serviceDetailsRequests, 1L);

        // Assert
        verify(jsonHelper, atLeast(1)).convertToJson(isA(ServiceDetails.class));
        verify(jsonHelper).readFromJson(any(), isA(Class.class));
        verify(iServiceDetailsRepository).findById(any());
        verify(iAuditLogService).addAuditLog(isA(AuditLogMetaData.class));
        verify(validatorUtility).applyValidation(anyString(),anyString(), any(),
                anyBoolean());
        verify(iServiceDetailsRepository).save(isA(ServiceDetails.class));
        assertEquals(1, actualSaveEntityFromShipmentResult.size());
    }

    /**
     * Method under test:
     * {@link ServiceDetailsDao#saveEntityFromShipment(List, Long)}
     */
    @Test
    void testSaveEntityFromShipment3() {
        // Arrange
        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");
        Class<Object> forNameResult = Object.class;
        when(jsonHelper.readFromJson(Mockito.<String>any(), Mockito.<Class<Object>>any())).thenReturn(forNameResult);
        when(jsonHelper.convertToJson(Mockito.<ServiceDetails>any())).thenReturn("Convert To Json");

        Parties contractor2 = new Parties();
        contractor2.setAddressCode("42 Main St");
        contractor2.setAddressData(new HashMap<>());
        contractor2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor2.setEntityId(1L);
        contractor2.setEntityType("Entity Type");
        contractor2.setGuid(UUID.randomUUID());
        contractor2.setId(1L);
        contractor2.setIsAddressFreeText(true);
        contractor2.setIsDeleted(true);
        contractor2.setOrgCode("Org Code");
        contractor2.setOrgData(new HashMap<>());
        contractor2.setTenantId(1);
        contractor2.setType("Type");
        contractor2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails2 = new ServiceDetails();
        serviceDetails2.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setConsolidationId(1L);
        serviceDetails2.setContractor(contractor2);
        serviceDetails2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails2.setGuid(UUID.randomUUID());
        serviceDetails2.setId(1L);
        serviceDetails2.setIsDeleted(true);
        serviceDetails2.setRefNumber("42");
        serviceDetails2.setServiceCount(3L);
        serviceDetails2.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails2.setServiceNotes("Service Notes");
        serviceDetails2.setServiceType("Service Type");
        serviceDetails2.setShipmentId(1L);
        serviceDetails2.setSrvLocation("Srv Location");
        serviceDetails2.setTenantId(1);
        serviceDetails2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setUpdatedBy("2020-03-01");
        Optional<ServiceDetails> ofResult = Optional.of(serviceDetails2);
        when(iServiceDetailsRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        when(validatorUtility.applyValidation(Mockito.<String>any(), Mockito.<String>any(), Mockito.<LifecycleHooks>any(),
                anyBoolean())).thenThrow(new ValidationException("SERVICE_DETAILS"));

        Parties contractor3 = new Parties();
        contractor3.setAddressCode("42 Main St");
        contractor3.setAddressData(new HashMap<>());
        contractor3.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor3.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor3.setEntityId(1L);
        contractor3.setEntityType("Entity Type");
        contractor3.setGuid(UUID.randomUUID());
        contractor3.setId(1L);
        contractor3.setIsAddressFreeText(true);
        contractor3.setIsDeleted(true);
        contractor3.setOrgCode("Org Code");
        contractor3.setOrgData(new HashMap<>());
        contractor3.setTenantId(1);
        contractor3.setType("Type");
        contractor3.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor3.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails3 = new ServiceDetails();
        serviceDetails3.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setConsolidationId(1L);
        serviceDetails3.setContractor(contractor3);
        serviceDetails3.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails3.setGuid(UUID.randomUUID());
        serviceDetails3.setId(1L);
        serviceDetails3.setIsDeleted(true);
        serviceDetails3.setRefNumber("42");
        serviceDetails3.setServiceCount(3L);
        serviceDetails3.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails3.setServiceNotes("Service Notes");
        serviceDetails3.setServiceType("Service Type");
        serviceDetails3.setShipmentId(1L);
        serviceDetails3.setSrvLocation("Srv Location");
        serviceDetails3.setTenantId(1);
        serviceDetails3.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails3.setUpdatedBy("2020-03-01");

        ArrayList<ServiceDetails> serviceDetailsRequests = new ArrayList<>();
        serviceDetailsRequests.add(serviceDetails3);

        // Act and Assert
        assertThrows(ValidationException.class, () -> serviceDetailsDao.saveEntityFromShipment(serviceDetailsRequests, 1L));
        verify(jsonHelper, atLeast(1)).convertToJson(isA(ServiceDetails.class));
        verify(iServiceDetailsRepository).findById(any());
        verify(validatorUtility).applyValidation(anyString(),anyString(), any(),
                anyBoolean());
    }

    /**
     * Method under test:
     * {@link ServiceDetailsDao#saveEntityFromShipment(List, Long, Map)}
     */
    @Test
    void testSaveEntityFromShipment4() {
        // Arrange
        ArrayList<ServiceDetails> serviceDetailsList = new ArrayList<>();
        when(iServiceDetailsRepository.saveAll(Mockito.<Iterable<ServiceDetails>>any())).thenReturn(serviceDetailsList);
        ArrayList<ServiceDetails> serviceDetailsRequests = new ArrayList<>();

        // Act
        List<ServiceDetails> actualSaveEntityFromShipmentResult = serviceDetailsDao
                .saveEntityFromShipment(serviceDetailsRequests, 1L, new HashMap<>());

        // Assert
        verify(iServiceDetailsRepository).saveAll(isA(Iterable.class));
        assertTrue(actualSaveEntityFromShipmentResult.isEmpty());
        assertSame(serviceDetailsList, actualSaveEntityFromShipmentResult);
    }

    /**
     * Method under test:
     * {@link ServiceDetailsDao#saveEntityFromShipment(List, Long, Map)}
     */
    @Test
    void testSaveEntityFromShipment5() throws RunnerException, JsonProcessingException, IllegalAccessException,
            NoSuchFieldException, NoSuchMethodException, InvocationTargetException {
        // Arrange
        doNothing().when(iAuditLogService).addAuditLog(Mockito.<AuditLogMetaData>any());

        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");

        ArrayList<ServiceDetails> serviceDetailsList = new ArrayList<>();
        serviceDetailsList.add(serviceDetails);
        when(iServiceDetailsRepository.saveAll(Mockito.<Iterable<ServiceDetails>>any())).thenReturn(serviceDetailsList);
        ArrayList<ServiceDetails> serviceDetailsRequests = new ArrayList<>();

        // Act
        List<ServiceDetails> actualSaveEntityFromShipmentResult = serviceDetailsDao
                .saveEntityFromShipment(serviceDetailsRequests, 1L, new HashMap<>());

        // Assert
        verify(iAuditLogService).addAuditLog(isA(AuditLogMetaData.class));
        verify(iServiceDetailsRepository).saveAll(isA(Iterable.class));
        assertEquals(1, actualSaveEntityFromShipmentResult.size());
        assertSame(serviceDetailsList, actualSaveEntityFromShipmentResult);
    }

    /**
     * Method under test:
     * {@link ServiceDetailsDao#saveEntityFromShipment(List, Long, Map)}
     */
    @Test
    void testSaveEntityFromShipment6() {
        // Arrange
        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");

        ArrayList<ServiceDetails> serviceDetailsRequests = new ArrayList<>();
        serviceDetailsRequests.add(serviceDetails);

        // Act and Assert
        Exception e = assertThrows(DataRetrievalFailureException.class,
                () -> serviceDetailsDao.saveEntityFromShipment(serviceDetailsRequests, 1L, new HashMap<>()));
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), e.getClass().getSimpleName());
    }

    /**
     * Method under test:
     * {@link ServiceDetailsDao#saveEntityFromShipment(List, Long, Map)}
     */
    @Test
    void testSaveEntityFromShipment7() {
        // Arrange
        Parties contractor = new Parties();
        contractor.setAddressCode("42 Main St");
        contractor.setAddressData(new HashMap<>());
        contractor.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        contractor.setEntityId(1L);
        contractor.setEntityType("Entity Type");
        contractor.setGuid(UUID.randomUUID());
        contractor.setId(1L);
        contractor.setIsAddressFreeText(true);
        contractor.setIsDeleted(true);
        contractor.setOrgCode("Org Code");
        contractor.setOrgData(new HashMap<>());
        contractor.setTenantId(1);
        contractor.setType("Type");
        contractor.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor.setUpdatedBy("2020-03-01");

        ServiceDetails serviceDetails = new ServiceDetails();
        serviceDetails.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setConsolidationId(1L);
        serviceDetails.setContractor(contractor);
        serviceDetails.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        serviceDetails.setGuid(UUID.randomUUID());
        serviceDetails.setId(1L);
        serviceDetails.setIsDeleted(true);
        serviceDetails.setRefNumber("42");
        serviceDetails.setServiceCount(3L);
        serviceDetails.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails.setServiceNotes("Service Notes");
        serviceDetails.setServiceType("Service Type");
        serviceDetails.setShipmentId(1L);
        serviceDetails.setSrvLocation("Srv Location");
        serviceDetails.setTenantId(1);
        serviceDetails.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails.setUpdatedBy("2020-03-01");

        Parties contractor2 = new Parties();
        contractor2.setAddressCode("17 High St");
        contractor2.setAddressData(new HashMap<>());
        contractor2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setCreatedBy("ServiceDetails is null for Id {}");
        contractor2.setEntityId(2L);
        contractor2.setEntityType("Failed to fetch data for given constraint.");
        contractor2.setGuid(UUID.randomUUID());
        contractor2.setId(2L);
        contractor2.setIsAddressFreeText(false);
        contractor2.setIsDeleted(false);
        contractor2.setOrgCode("Failed to fetch data for given constraint.");
        contractor2.setOrgData(new HashMap<>());
        contractor2.setTenantId(2);
        contractor2.setType("Failed to fetch data for given constraint.");
        contractor2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        contractor2.setUpdatedBy("2020/03/01");

        ServiceDetails serviceDetails2 = new ServiceDetails();
        serviceDetails2.setBookingDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCompletionDate(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setConsolidationId(2L);
        serviceDetails2.setContractor(contractor2);
        serviceDetails2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setCreatedBy("ServiceDetails is null for Id {}");
        serviceDetails2.setGuid(UUID.randomUUID());
        serviceDetails2.setId(2L);
        serviceDetails2.setIsDeleted(false);
        serviceDetails2.setRefNumber("ServiceDetails is null for Id {}");
        serviceDetails2.setServiceCount(1L);
        serviceDetails2.setServiceDuration(LocalTime.MIDNIGHT);
        serviceDetails2.setServiceNotes("Failed to fetch data for given constraint.");
        serviceDetails2.setServiceType("Failed to fetch data for given constraint.");
        serviceDetails2.setShipmentId(2L);
        serviceDetails2.setSrvLocation("Failed to fetch data for given constraint.");
        serviceDetails2.setTenantId(2);
        serviceDetails2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        serviceDetails2.setUpdatedBy("2020/03/01");

        ArrayList<ServiceDetails> serviceDetailsRequests = new ArrayList<>();
        serviceDetailsRequests.add(serviceDetails2);
        serviceDetailsRequests.add(serviceDetails);

        // Act and Assert
        Exception e = assertThrows(DataRetrievalFailureException.class,
                () -> serviceDetailsDao.saveEntityFromShipment(serviceDetailsRequests, 1L, new HashMap<>()));
        assertEquals(DataRetrievalFailureException.class.getSimpleName(), e.getClass().getSimpleName());
    }

    @Test
    void saveTestWithValidationFailed() {
        when(validatorUtility.applyValidation(any(), anyString(), any(), anyBoolean())).thenReturn(Set.of(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE));
        Exception e = assertThrows(ValidationException.class, () -> serviceDetailsDao.save(null));

        assertEquals(ValidationException.class.getSimpleName(), e.getClass().getSimpleName());
    }

    @Test
    void updateEntityFromShipmentThrowsException() throws RunnerException {
        var inputServices = completeShipment.getServicesList();
        inputServices.add(new ServiceDetails());
        Exception e = assertThrows(RunnerException.class, () -> serviceDetailsDao.updateEntityFromShipment(inputServices, completeShipment.getId()));
        assertEquals(RunnerException.class.getSimpleName(), e.getClass().getSimpleName());
    }

    @Test
    void updateEntityFromShipmentWithExtraServicesAdded() throws RunnerException {
        var inputServices = completeShipment.getServicesList();
        inputServices.add(new ServiceDetails());
        when(iServiceDetailsRepository.findByShipmentId(any()))
                .thenReturn(completeShipment.getServicesList());
        var servicesResponse = serviceDetailsDao.updateEntityFromShipment(inputServices, completeShipment.getId());
        assertNotNull(servicesResponse);
    }

    @Test
    void saveEntityFromShipmentThrowsException() throws RunnerException {
        var inputServices = completeShipment.getServicesList();
        when(iServiceDetailsRepository.findById(anyLong())).thenReturn(Optional.empty());
        Exception e = assertThrows(DataRetrievalFailureException.class, () -> serviceDetailsDao.saveEntityFromShipment(inputServices, completeShipment.getId()));
        assertNotNull(DataRetrievalFailureException.class.getSimpleName(), e.getClass().getSimpleName());
    }

    @Test
    void saveEntityFromShipmentAuditLogServiceThrowsException() throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException {
        var inputServices = completeShipment.getServicesList();
        inputServices.add(new ServiceDetails());
        PageImpl<ServiceDetails> pageImpl = new PageImpl<>(completeShipment.getServicesList());
        when(iServiceDetailsRepository.findAll(Mockito.<Specification<ServiceDetails>>any(), Mockito.<Pageable>any()))
                .thenReturn(pageImpl);
        when(iServiceDetailsRepository.findById(anyLong())).thenReturn(Optional.of(inputServices.get(0)));
        doThrow(new RuntimeException("RuntimeException")).when(iAuditLogService).addAuditLog(any());
        var servicesResponse = serviceDetailsDao.saveEntityFromShipment(inputServices, completeShipment.getId());
        assertNotNull(servicesResponse);
    }

    @Test
    void updateEntityFromShipmentWithServiceMap() throws Exception {
        var inputServices = completeShipment.getServicesList();
        inputServices.add(new ServiceDetails());
        PageImpl<ServiceDetails> pageImpl = new PageImpl<>(completeShipment.getServicesList());
        when(iServiceDetailsRepository.findAll(Mockito.<Specification<ServiceDetails>>any(), Mockito.<Pageable>any()))
                .thenReturn(pageImpl);
        when(iServiceDetailsRepository.saveAll(any())).thenReturn(inputServices);
        doThrow(new RuntimeException("RuntimeException")).when(iAuditLogService).addAuditLog(any());
        var servicesResponse = serviceDetailsDao.saveEntityFromShipment(inputServices, completeShipment.getId(), completeShipment.getServicesList().stream().collect(Collectors.toMap(ServiceDetails::getId, Function.identity())));
        assertNotNull(servicesResponse);
    }

    @Test
    void updateEntityFromShipmentWithDeletingExistingService() throws Exception {
        var inputServices = completeShipment.getServicesList();
        inputServices.add(new ServiceDetails());
        PageImpl<ServiceDetails> pageImpl = new PageImpl<>(completeShipment.getServicesList());
        when(iServiceDetailsRepository.findAll(Mockito.<Specification<ServiceDetails>>any(), Mockito.<Pageable>any()))
                .thenReturn(pageImpl);
        when(iServiceDetailsRepository.saveAll(any())).thenReturn(inputServices);
        doThrow(new RuntimeException("RuntimeException")).when(iAuditLogService).addAuditLog(any());
        Exception e = assertThrows(RunnerException.class, () -> serviceDetailsDao.updateEntityFromShipment(inputServices, completeShipment.getId(), completeShipment.getServicesList()));
        assertNotNull(RunnerException.class.getSimpleName(), e.getClass().getSimpleName());
    }
}
