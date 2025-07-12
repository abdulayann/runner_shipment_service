package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.enums.TILegType;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ITiContainerDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsContainersListRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsContainersRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsContainersListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsContainersResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.TiContainers;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ITiLegRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.ITransportInstructionLegsContainersService;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.v3.TransportInstructionValidationUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class TransportInstructionLegsContainersServiceImplTest {
    @MockBean
    private DependentServiceHelper dependentServiceHelper;

    @MockBean
    private IAuditLogService iAuditLogService;

    @MockBean
    private IContainerV3Service iContainerV3Service;

    @MockBean
    private ITiContainerDao iTiContainerDao;

    @MockBean
    private ITiLegRepository iTiLegRepository;

    @MockBean
    private JsonHelper jsonHelper;

    @MockBean
    private UserContext userContext;

    @InjectMocks
    private TransportInstructionLegsContainersServiceImpl transportInstructionLegsContainersServiceImpl;
    @MockBean
    private MasterDataUtils masterDataUtils;

    @MockBean
    private TransportInstructionValidationUtil transportInstructionValidationUtil;

    @BeforeEach
    void setup() {
        transportInstructionLegsContainersServiceImpl.executorServiceMasterData = Executors.newFixedThreadPool(2);
    }

    @Test
    void testCreate() {
        ContainerNumberCheckResponse containerNumberCheckResponse = new ContainerNumberCheckResponse();
        containerNumberCheckResponse.setIsLastDigitCorrect(true);
        containerNumberCheckResponse.setLastDigit(1);
        containerNumberCheckResponse.setSuccess(true);
        when(iContainerV3Service.validateContainerNumber(Mockito.<String>any())).thenReturn(containerNumberCheckResponse);

        TiContainers tiContainers = new TiContainers();
        tiContainers.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiContainers.setDangerous(true);
        tiContainers.setDescription("The characteristics of someone or something");
        tiContainers.setDgClass("Dg Class");
        tiContainers.setGrossWeight(new BigDecimal("2.3"));
        tiContainers.setGrossWeightUnit("Gross Weight Unit");
        tiContainers.setGuid(UUID.randomUUID());
        tiContainers.setId(1L);
        tiContainers.setIsDeleted(true);
        tiContainers.setNetWeight(new BigDecimal("2.3"));
        tiContainers.setNetWeightUnit("Net Weight Unit");
        tiContainers.setNoOfPackages("java.text");
        tiContainers.setNumber("42");
        tiContainers.setSubstanceName("Substance Name");
        tiContainers.setTenantId(1);
        tiContainers.setTiLegId(1L);
        tiContainers.setTunnelRestrictionCode("Tunnel Restriction Code");
        tiContainers.setType("Type");
        tiContainers.setUnNumber("42");
        tiContainers.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers.setUpdatedBy("2020-03-01");
        tiContainers.setVolume(new BigDecimal("2.3"));
        tiContainers.setVolumeUnit("Volume Unit");
        when(jsonHelper.convertValue(Mockito.<TransportInstructionLegsContainersRequest>any(),
                Mockito.<Class<TiContainers>>any())).thenReturn(tiContainers);
        when(iTiContainerDao.save(Mockito.<TiContainers>any())).thenThrow(new ValidationException("request_id"));

        Parties destination = new Parties();
        destination.setAddressCode("42 Main St");
        destination.setAddressData(new HashMap<>());
        destination.setAddressId("42 Main St");
        destination.setCountryCode("GB");
        destination.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        destination.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        destination.setEntityId(1L);
        destination.setEntityType("Entity Type");
        destination.setGuid(UUID.randomUUID());
        destination.setId(1L);
        destination.setIsAddressFreeText(true);
        destination.setIsDeleted(true);
        destination.setOrgCode("Org Code");
        destination.setOrgData(new HashMap<>());
        destination.setOrgId("42");
        destination.setTenantId(1);
        destination.setType("Type");
        destination.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        destination.setUpdatedBy("2020-03-01");

        Parties origin = new Parties();
        origin.setAddressCode("42 Main St");
        origin.setAddressData(new HashMap<>());
        origin.setAddressId("42 Main St");
        origin.setCountryCode("GB");
        origin.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        origin.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        origin.setEntityId(1L);
        origin.setEntityType("Entity Type");
        origin.setGuid(UUID.randomUUID());
        origin.setId(1L);
        origin.setIsAddressFreeText(true);
        origin.setIsDeleted(true);
        origin.setOrgCode("Org Code");
        origin.setOrgData(new HashMap<>());
        origin.setOrgId("42");
        origin.setTenantId(1);
        origin.setType("Type");
        origin.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        origin.setUpdatedBy("2020-03-01");

        TiLegs tiLegs = new TiLegs();
        tiLegs.setActualDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setActualPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiLegs.setDestination(destination);
        tiLegs.setDropMode("Drop Mode");
        tiLegs.setEstimatedDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setEstimatedPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setGuid(UUID.randomUUID());
        tiLegs.setId(1L);
        tiLegs.setIsDeleted(true);
        tiLegs.setLegType(TILegType.EMPTY);
        tiLegs.setOrigin(origin);
        tiLegs.setPickupDeliveryDetailsId(1L);
        tiLegs.setRemarks("Remarks");
        tiLegs.setRequiredBy(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setSequence(1L);
        tiLegs.setTenantId(1);
        tiLegs.setTiContainers(new ArrayList<>());
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiReferences(new ArrayList<>());
        tiLegs.setTiTruckDriverDetails(new ArrayList<>());
        tiLegs.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setUpdatedBy("2020-03-01");
        Optional<TiLegs> ofResult = Optional.of(tiLegs);
        TransportInstructionLegsContainersRequest containersRequest = new TransportInstructionLegsContainersRequest();
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        assertThrows(ValidationException.class, () ->
                transportInstructionLegsContainersServiceImpl.create(containersRequest)
        );
        verify(iTiContainerDao).save(Mockito.<TiContainers>any());
        verify(jsonHelper).convertValue(Mockito.<TransportInstructionLegsContainersRequest>any(),
                Mockito.<Class<TiContainers>>any());
        verify(iTiLegRepository).findById(Mockito.<Long>any());
    }

    /**
     * Method under test:
     * {@link TransportInstructionLegsContainersServiceImpl#create(TransportInstructionLegsContainersRequest)}
     */
    @Test
    void testCreate2() {
        ContainerNumberCheckResponse containerNumberCheckResponse = new ContainerNumberCheckResponse();
        containerNumberCheckResponse.setIsLastDigitCorrect(true);
        containerNumberCheckResponse.setLastDigit(1);
        containerNumberCheckResponse.setSuccess(true);
        when(iContainerV3Service.validateContainerNumber(Mockito.<String>any())).thenReturn(containerNumberCheckResponse);

        Parties destination = new Parties();
        destination.setAddressCode("42 Main St");
        destination.setAddressData(new HashMap<>());
        destination.setAddressId("42 Main St");
        destination.setCountryCode("GB");
        destination.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        destination.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        destination.setEntityId(1L);
        destination.setEntityType("Entity Type");
        destination.setGuid(UUID.randomUUID());
        destination.setId(1L);
        destination.setIsAddressFreeText(true);
        destination.setIsDeleted(true);
        destination.setOrgCode("Org Code");
        destination.setOrgData(new HashMap<>());
        destination.setOrgId("42");
        destination.setTenantId(1);
        destination.setType("Type");
        destination.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        destination.setUpdatedBy("2020-03-01");

        Parties origin = new Parties();
        origin.setAddressCode("42 Main St");
        origin.setAddressData(new HashMap<>());
        origin.setAddressId("42 Main St");
        origin.setCountryCode("GB");
        origin.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        origin.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        origin.setEntityId(1L);
        origin.setEntityType("Entity Type");
        origin.setGuid(UUID.randomUUID());
        origin.setId(1L);
        origin.setIsAddressFreeText(true);
        origin.setIsDeleted(true);
        origin.setOrgCode("Org Code");
        origin.setOrgData(new HashMap<>());
        origin.setOrgId("42");
        origin.setTenantId(1);
        origin.setType("Type");
        origin.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        origin.setUpdatedBy("2020-03-01");

        TiLegs tiLegs = new TiLegs();
        tiLegs.setActualDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setActualPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiLegs.setDestination(destination);
        tiLegs.setDropMode("Drop Mode");
        tiLegs.setEstimatedDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setEstimatedPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setGuid(UUID.randomUUID());
        tiLegs.setId(1L);
        tiLegs.setIsDeleted(true);
        tiLegs.setLegType(TILegType.EMPTY);
        tiLegs.setOrigin(origin);
        tiLegs.setPickupDeliveryDetailsId(1L);
        tiLegs.setRemarks("Remarks");
        tiLegs.setRequiredBy(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setSequence(1L);
        tiLegs.setTenantId(1);
        tiLegs.setTiContainers(new ArrayList<>());
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiReferences(new ArrayList<>());
        tiLegs.setTiTruckDriverDetails(new ArrayList<>());
        tiLegs.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setUpdatedBy("2020-03-01");
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(Optional.empty());
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder dgClassResult = TransportInstructionLegsContainersRequest
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something")
                .dgClass("Dg Class");
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("200000004506064040054.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .number("42")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .type("Type")
                .unNumber("42");
        TransportInstructionLegsContainersRequest request = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit")
                .build();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        when(iTiContainerDao.save(any())).thenReturn(new TiContainers());
        when(jsonHelper.convertValue(request, TiContainers.class)).thenReturn(new TiContainers());
        assertThrows(ValidationException.class, () -> transportInstructionLegsContainersServiceImpl.create(request));
        verify(iTiLegRepository).findById(Mockito.<Long>any());
    }

    /**
     * Method under test:
     * {@link TransportInstructionLegsContainersServiceImpl#create(TransportInstructionLegsContainersRequest)}
     */
    @Test
    void testCreate3() {
        ContainerNumberCheckResponse containerNumberCheckResponse = mock(ContainerNumberCheckResponse.class);
        when(containerNumberCheckResponse.isSuccess()).thenReturn(false);
        doNothing().when(containerNumberCheckResponse).setIsLastDigitCorrect(Mockito.<Boolean>any());
        doNothing().when(containerNumberCheckResponse).setLastDigit(Mockito.<Integer>any());
        doNothing().when(containerNumberCheckResponse).setSuccess(anyBoolean());
        containerNumberCheckResponse.setIsLastDigitCorrect(true);
        containerNumberCheckResponse.setLastDigit(1);
        containerNumberCheckResponse.setSuccess(true);
        when(iContainerV3Service.validateContainerNumber(Mockito.<String>any())).thenReturn(containerNumberCheckResponse);

        Parties destination = new Parties();
        destination.setAddressCode("42 Main St");
        destination.setAddressData(new HashMap<>());
        destination.setAddressId("42 Main St");
        destination.setCountryCode("GB");
        destination.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        destination.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        destination.setEntityId(1L);
        destination.setEntityType("Entity Type");
        destination.setGuid(UUID.randomUUID());
        destination.setId(1L);
        destination.setIsAddressFreeText(true);
        destination.setIsDeleted(true);
        destination.setOrgCode("Org Code");
        destination.setOrgData(new HashMap<>());
        destination.setOrgId("42");
        destination.setTenantId(1);
        destination.setType("Type");
        destination.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        destination.setUpdatedBy("2020-03-01");

        Parties origin = new Parties();
        origin.setAddressCode("42 Main St");
        origin.setAddressData(new HashMap<>());
        origin.setAddressId("42 Main St");
        origin.setCountryCode("GB");
        origin.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        origin.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        origin.setEntityId(1L);
        origin.setEntityType("Entity Type");
        origin.setGuid(UUID.randomUUID());
        origin.setId(1L);
        origin.setIsAddressFreeText(true);
        origin.setIsDeleted(true);
        origin.setOrgCode("Org Code");
        origin.setOrgData(new HashMap<>());
        origin.setOrgId("42");
        origin.setTenantId(1);
        origin.setType("Type");
        origin.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        origin.setUpdatedBy("2020-03-01");

        TiLegs tiLegs = new TiLegs();
        tiLegs.setActualDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setActualPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiLegs.setDestination(destination);
        tiLegs.setDropMode("Drop Mode");
        tiLegs.setEstimatedDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setEstimatedPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setGuid(UUID.randomUUID());
        tiLegs.setId(1L);
        tiLegs.setIsDeleted(true);
        tiLegs.setLegType(TILegType.EMPTY);
        tiLegs.setOrigin(origin);
        tiLegs.setPickupDeliveryDetailsId(1L);
        tiLegs.setRemarks("Remarks");
        tiLegs.setRequiredBy(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setSequence(1L);
        tiLegs.setTenantId(1);
        tiLegs.setTiContainers(new ArrayList<>());
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiReferences(new ArrayList<>());
        tiLegs.setTiTruckDriverDetails(new ArrayList<>());
        tiLegs.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setUpdatedBy("2020-03-01");
        Optional<TiLegs> ofResult = Optional.of(tiLegs);
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder dgClassResult = TransportInstructionLegsContainersRequest
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something")
                .dgClass("Dg Class");
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .number("42")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .type("Type")
                .unNumber("42");
        TransportInstructionLegsContainersRequest request = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit")
                .build();
        assertThrows(ValidationException.class, () -> transportInstructionLegsContainersServiceImpl.create(request));
        verify(containerNumberCheckResponse).isSuccess();
        verify(containerNumberCheckResponse).setIsLastDigitCorrect(Mockito.<Boolean>any());
        verify(containerNumberCheckResponse).setLastDigit(Mockito.<Integer>any());
        verify(containerNumberCheckResponse).setSuccess(anyBoolean());
        verify(iTiLegRepository).findById(Mockito.<Long>any());
        verify(iContainerV3Service).validateContainerNumber(Mockito.<String>any());
    }

    @Test
    void testCreate4() throws RunnerException, JsonProcessingException, IllegalAccessException, NoSuchFieldException,
            NoSuchMethodException, InvocationTargetException {
        ContainerNumberCheckResponse containerNumberCheckResponse = new ContainerNumberCheckResponse();
        containerNumberCheckResponse.setSuccess(true);
        when(iContainerV3Service.validateContainerNumber("CONT1234567")).thenReturn(containerNumberCheckResponse);

        Parties destination = new Parties();
        destination.setAddressCode("42 Main St");
        destination.setAddressData(new HashMap<>());
        destination.setAddressId("42 Main St");
        destination.setCountryCode("GB");
        destination.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        destination.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        destination.setEntityId(1L);
        destination.setEntityType("Entity Type");
        destination.setGuid(UUID.randomUUID());
        destination.setId(1L);
        destination.setIsAddressFreeText(true);
        destination.setIsDeleted(true);
        destination.setOrgCode("Org Code");
        destination.setOrgData(new HashMap<>());
        destination.setOrgId("42");
        destination.setTenantId(1);
        destination.setType("Type");
        destination.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        destination.setUpdatedBy("2020-03-01");

        Parties origin = new Parties();
        origin.setAddressCode("42 Main St");
        origin.setAddressData(new HashMap<>());
        origin.setAddressId("42 Main St");
        origin.setCountryCode("GB");
        origin.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        origin.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        origin.setEntityId(1L);
        origin.setEntityType("Entity Type");
        origin.setGuid(UUID.randomUUID());
        origin.setId(1L);
        origin.setIsAddressFreeText(true);
        origin.setIsDeleted(true);
        origin.setOrgCode("Org Code");
        origin.setOrgData(new HashMap<>());
        origin.setOrgId("42");
        origin.setTenantId(1);
        origin.setType("Type");
        origin.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        origin.setUpdatedBy("2020-03-01");

        TiLegs tiLegs = new TiLegs();
        tiLegs.setActualDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setActualPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiLegs.setDestination(destination);
        tiLegs.setDropMode("Drop Mode");
        tiLegs.setEstimatedDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setEstimatedPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setGuid(UUID.randomUUID());
        tiLegs.setId(1L);
        tiLegs.setIsDeleted(true);
        tiLegs.setLegType(TILegType.EMPTY);
        tiLegs.setOrigin(origin);
        tiLegs.setPickupDeliveryDetailsId(1L);
        tiLegs.setRemarks("Remarks");
        tiLegs.setRequiredBy(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setSequence(1L);
        tiLegs.setTenantId(1);
        tiLegs.setTiContainers(new ArrayList<>());
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiReferences(new ArrayList<>());
        tiLegs.setTiTruckDriverDetails(new ArrayList<>());
        tiLegs.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setUpdatedBy("2020-03-01");
        Optional<TiLegs> ofResult = Optional.of(tiLegs);
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder dgClassResult = TransportInstructionLegsContainersRequest
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something")
                .dgClass("Dg Class");
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("KG");
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("KG")
                .noOfPackages("20")
                .packageType("BAG")
                .number("CONT1234567")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .type("EMPTY")
                .unNumber("42");
        TransportInstructionLegsContainersRequest request = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("M3")
                .build();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsContainersResponse.class))).thenReturn(new TransportInstructionLegsContainersResponse());
        when(iTiContainerDao.save(any())).thenReturn(new TiContainers());
        when(jsonHelper.convertValue(request, TiContainers.class)).thenReturn(new TiContainers());
        TransportInstructionLegsContainersResponse response = transportInstructionLegsContainersServiceImpl.create(request);
        assertNotNull(response);
        verify(iTiLegRepository).findById(Mockito.<Long>any());
        verify(iContainerV3Service).validateContainerNumber(Mockito.<String>any());
    }

    @Test
    void testCreate5() {
        ContainerNumberCheckResponse containerNumberCheckResponse = new ContainerNumberCheckResponse();
        containerNumberCheckResponse.setSuccess(true);
        when(iContainerV3Service.validateContainerNumber("CONT1234567")).thenReturn(containerNumberCheckResponse);
        TiContainers tiContainers = new TiContainers();
        tiContainers.setId(1l);
        tiContainers.setNumber("CONT1234567");
        TiLegs tiLegs = new TiLegs();
        tiLegs.setId(1l);
        tiLegs.setActualDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setActualPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiLegs.setDropMode("Drop Mode");
        tiLegs.setEstimatedDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setEstimatedPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setGuid(UUID.randomUUID());
        tiLegs.setId(1L);
        tiLegs.setIsDeleted(true);
        tiLegs.setLegType(TILegType.EMPTY);
        tiLegs.setPickupDeliveryDetailsId(1L);
        tiLegs.setRemarks("Remarks");
        tiLegs.setRequiredBy(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setSequence(1L);
        tiLegs.setTenantId(1);
        tiLegs.setTiContainers(List.of(tiContainers));
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiReferences(new ArrayList<>());
        tiLegs.setTiTruckDriverDetails(new ArrayList<>());
        tiLegs.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setUpdatedBy("2020-03-01");
        Optional<TiLegs> ofResult = Optional.of(tiLegs);
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder dgClassResult = TransportInstructionLegsContainersRequest
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something")
                .dgClass("Dg Class");
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("KG");
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID());

        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("KG")
                .noOfPackages("20")
                .number("CONT1234567")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .type("EMPTY")
                .unNumber("42");
        TransportInstructionLegsContainersRequest request = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("M3")
                .build();
        assertThrows(ValidationException.class, () -> transportInstructionLegsContainersServiceImpl.create(request));
    }

    @Test
    void testCreateBulk() throws RunnerException, JsonProcessingException, IllegalAccessException, NoSuchFieldException,
            NoSuchMethodException, InvocationTargetException {
        ContainerNumberCheckResponse containerNumberCheckResponse = new ContainerNumberCheckResponse();
        containerNumberCheckResponse.setSuccess(true);
        when(iContainerV3Service.validateContainerNumber("CONT1234567")).thenReturn(containerNumberCheckResponse);

        Parties destination = new Parties();
        destination.setAddressCode("42 Main St");
        destination.setAddressData(new HashMap<>());
        destination.setAddressId("42 Main St");
        destination.setCountryCode("GB");
        destination.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        destination.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        destination.setEntityId(1L);
        destination.setEntityType("Entity Type");
        destination.setGuid(UUID.randomUUID());
        destination.setId(1L);
        destination.setIsAddressFreeText(true);
        destination.setIsDeleted(true);
        destination.setOrgCode("Org Code");
        destination.setOrgData(new HashMap<>());
        destination.setOrgId("42");
        destination.setTenantId(1);
        destination.setType("Type");
        destination.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        destination.setUpdatedBy("2020-03-01");

        Parties origin = new Parties();
        origin.setAddressCode("42 Main St");
        origin.setAddressData(new HashMap<>());
        origin.setAddressId("42 Main St");
        origin.setCountryCode("GB");
        origin.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        origin.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        origin.setEntityId(1L);
        origin.setEntityType("Entity Type");
        origin.setGuid(UUID.randomUUID());
        origin.setId(1L);
        origin.setIsAddressFreeText(true);
        origin.setIsDeleted(true);
        origin.setOrgCode("Org Code");
        origin.setOrgData(new HashMap<>());
        origin.setOrgId("42");
        origin.setTenantId(1);
        origin.setType("Type");
        origin.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        origin.setUpdatedBy("2020-03-01");

        TiLegs tiLegs = new TiLegs();
        tiLegs.setActualDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setActualPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiLegs.setDestination(destination);
        tiLegs.setDropMode("Drop Mode");
        tiLegs.setEstimatedDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setEstimatedPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setGuid(UUID.randomUUID());
        tiLegs.setId(1L);
        tiLegs.setIsDeleted(true);
        tiLegs.setLegType(TILegType.EMPTY);
        tiLegs.setOrigin(origin);
        tiLegs.setPickupDeliveryDetailsId(1L);
        tiLegs.setRemarks("Remarks");
        tiLegs.setRequiredBy(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setSequence(1L);
        tiLegs.setTenantId(1);
        tiLegs.setTiContainers(new ArrayList<>());
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiReferences(new ArrayList<>());
        tiLegs.setTiTruckDriverDetails(new ArrayList<>());
        tiLegs.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setUpdatedBy("2020-03-01");
        Optional<TiLegs> ofResult = Optional.of(tiLegs);
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder dgClassResult = TransportInstructionLegsContainersRequest
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something")
                .dgClass("Dg Class");
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("KG");
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("KG")
                .noOfPackages("20")
                .packageType("BAG")
                .number("CONT1234567")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .type("EMPTY")
                .unNumber("42");
        TransportInstructionLegsContainersRequest request = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("M3")
                .build();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsContainersResponse.class))).thenReturn(new TransportInstructionLegsContainersResponse());
        when(iTiContainerDao.saveAll(any())).thenReturn(List.of(new TiContainers()));
        when(jsonHelper.convertValue(request, TiContainers.class)).thenReturn(new TiContainers());
        TransportInstructionLegsContainersListRequest containersListRequest = new TransportInstructionLegsContainersListRequest();
        containersListRequest.setContainersRequests(List.of(request));
        TransportInstructionLegsContainersListResponse response = transportInstructionLegsContainersServiceImpl.bulkCreate(containersListRequest);
        assertNotNull(response);
        verify(iTiLegRepository).findById(Mockito.<Long>any());
        verify(iContainerV3Service).validateContainerNumber(Mockito.<String>any());
    }

    @Test
    void testUpdate() throws RunnerException, JsonProcessingException, IllegalAccessException, NoSuchFieldException,
            NoSuchMethodException, InvocationTargetException {
        ContainerNumberCheckResponse containerNumberCheckResponse = new ContainerNumberCheckResponse();
        containerNumberCheckResponse.setSuccess(true);
        when(iContainerV3Service.validateContainerNumber("CONT1234567")).thenReturn(containerNumberCheckResponse);

        Parties destination = new Parties();
        destination.setAddressCode("42 Main St");
        destination.setAddressData(new HashMap<>());
        destination.setAddressId("42 Main St");
        destination.setCountryCode("GB");
        destination.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        destination.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        destination.setEntityId(1L);
        destination.setEntityType("Entity Type");
        destination.setGuid(UUID.randomUUID());
        destination.setId(1L);
        destination.setIsAddressFreeText(true);
        destination.setIsDeleted(true);
        destination.setOrgCode("Org Code");
        destination.setOrgData(new HashMap<>());
        destination.setOrgId("42");
        destination.setTenantId(1);
        destination.setType("Type");
        destination.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        destination.setUpdatedBy("2020-03-01");

        Parties origin = new Parties();
        origin.setAddressCode("42 Main St");
        origin.setAddressData(new HashMap<>());
        origin.setAddressId("42 Main St");
        origin.setCountryCode("GB");
        origin.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        origin.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        origin.setEntityId(1L);
        origin.setEntityType("Entity Type");
        origin.setGuid(UUID.randomUUID());
        origin.setId(1L);
        origin.setIsAddressFreeText(true);
        origin.setIsDeleted(true);
        origin.setOrgCode("Org Code");
        origin.setOrgData(new HashMap<>());
        origin.setOrgId("42");
        origin.setTenantId(1);
        origin.setType("Type");
        origin.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        origin.setUpdatedBy("2020-03-01");

        TiLegs tiLegs = new TiLegs();
        tiLegs.setActualDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setActualPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiLegs.setDestination(destination);
        tiLegs.setDropMode("Drop Mode");
        tiLegs.setEstimatedDelivery(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setEstimatedPickup(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setGuid(UUID.randomUUID());
        tiLegs.setId(1L);
        tiLegs.setIsDeleted(true);
        tiLegs.setLegType(TILegType.EMPTY);
        tiLegs.setOrigin(origin);
        tiLegs.setPickupDeliveryDetailsId(1L);
        tiLegs.setRemarks("Remarks");
        tiLegs.setRequiredBy(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setSequence(1L);
        tiLegs.setTenantId(1);
        tiLegs.setTiContainers(new ArrayList<>());
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiReferences(new ArrayList<>());
        tiLegs.setTiTruckDriverDetails(new ArrayList<>());
        tiLegs.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setUpdatedBy("2020-03-01");
        Optional<TiLegs> ofResult = Optional.of(tiLegs);
        TiContainers tiContainers = new TiContainers();
        tiContainers.setId(1L);
        when(iTiContainerDao.findById(1L)).thenReturn(Optional.of(tiContainers));
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder dgClassResult = TransportInstructionLegsContainersRequest
                .builder()
                .tiLegId(1L)
                .dangerous(true)
                .description("The characteristics of someone or something")
                .dgClass("Dg Class");
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("KG");
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsContainersRequest.TransportInstructionLegsContainersRequestBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("KG")
                .noOfPackages("20")
                .packageType("BAG")
                .number("CONT1234567")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .type("EMPTY")
                .unNumber("42");
        TransportInstructionLegsContainersRequest request = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("M3")
                .build();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsContainersResponse.class))).thenReturn(new TransportInstructionLegsContainersResponse());
        when(iTiContainerDao.save(any())).thenReturn(new TiContainers());
        when(jsonHelper.convertValue(request, TiContainers.class)).thenReturn(new TiContainers());
        TransportInstructionLegsContainersResponse response = transportInstructionLegsContainersServiceImpl.update(request);
        assertNotNull(response);
        verify(iTiLegRepository).findById(Mockito.<Long>any());
        verify(iContainerV3Service).validateContainerNumber(Mockito.<String>any());
    }

    /**
     * Method under test:
     * {@link ITransportInstructionLegsContainersService#list(ListCommonRequest, boolean)}
     */
    @Test
    void testList() {
        ArrayList<TiContainers> content = new ArrayList<>();
        when(iTiContainerDao.findAll(Mockito.<Specification<TiContainers>>any(), Mockito.<Pageable>any()))
                .thenReturn(new PageImpl<>(content));
        TransportInstructionLegsContainersListResponse actualListResult = transportInstructionLegsContainersServiceImpl
                .list(new ListCommonRequest(), true);
        verify(iTiContainerDao).findAll(Mockito.<Specification<TiContainers>>any(), Mockito.<Pageable>any());
        assertEquals(0L, actualListResult.getTotalCount().longValue());
        assertEquals(1, actualListResult.getTotalPages().intValue());
        assertEquals(content, actualListResult.getTiLegsContainersResponses());
    }

    /**
     * Method under test:
     * {@link ITransportInstructionLegsContainersService#list(ListCommonRequest, boolean)}
     */
    @Test
    void testList2() {
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder dgClassResult = TransportInstructionLegsContainersResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something")
                .dgClass("Dg Class");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .number("42")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .type("Type")
                .unNumber("42");
        TransportInstructionLegsContainersResponse buildResult = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit")
                .build();
        when(jsonHelper.convertValue(Mockito.<TiContainers>any(),
                Mockito.<Class<TransportInstructionLegsContainersResponse>>any())).thenReturn(buildResult);

        TiContainers tiContainers = new TiContainers();
        tiContainers.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiContainers.setDangerous(true);
        tiContainers.setDescription("The characteristics of someone or something");
        tiContainers.setDgClass("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers.setGrossWeight(new BigDecimal("2.3"));
        tiContainers
                .setGrossWeightUnit("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers.setGuid(UUID.randomUUID());
        tiContainers.setId(1L);
        tiContainers.setIsDeleted(true);
        tiContainers.setNetWeight(new BigDecimal("2.3"));
        tiContainers
                .setNetWeightUnit("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers.setNoOfPackages("java.text");
        tiContainers.setNumber("42");
        tiContainers
                .setSubstanceName("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers.setTenantId(1);
        tiContainers.setTiLegId(1L);
        tiContainers.setTunnelRestrictionCode(
                "Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers.setType("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers.setUnNumber("42");
        tiContainers.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers.setUpdatedBy("2020-03-01");
        tiContainers.setVolume(new BigDecimal("2.3"));
        tiContainers.setVolumeUnit("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");

        ArrayList<TiContainers> content = new ArrayList<>();
        content.add(tiContainers);
        PageImpl<TiContainers> pageImpl = new PageImpl<>(content);
        when(iTiContainerDao.findAll(Mockito.<Specification<TiContainers>>any(), Mockito.<Pageable>any()))
                .thenReturn(pageImpl);
        TransportInstructionLegsContainersListResponse actualListResult = transportInstructionLegsContainersServiceImpl
                .list(new ListCommonRequest(), true);
        verify(iTiContainerDao).findAll(Mockito.<Specification<TiContainers>>any(), Mockito.<Pageable>any());
        verify(jsonHelper).convertValue(Mockito.<TiContainers>any(),
                Mockito.<Class<TransportInstructionLegsContainersResponse>>any());
        assertEquals(1, actualListResult.getTotalPages().intValue());
        assertEquals(1, actualListResult.getTiLegsContainersResponses().size());
        assertEquals(1L, actualListResult.getTotalCount().longValue());
    }

    /**
     * Method under test:
     * {@link ITransportInstructionLegsContainersService#list(ListCommonRequest, boolean)}
     */
    @Test
    void testList3() {
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder dgClassResult = TransportInstructionLegsContainersResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something")
                .dgClass("Dg Class");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .number("42")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .type("Type")
                .unNumber("42");
        TransportInstructionLegsContainersResponse buildResult = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit")
                .build();
        when(jsonHelper.convertValue(Mockito.<TiContainers>any(),
                Mockito.<Class<TransportInstructionLegsContainersResponse>>any())).thenReturn(buildResult);

        TiContainers tiContainers = new TiContainers();
        tiContainers.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiContainers.setDangerous(true);
        tiContainers.setDescription("The characteristics of someone or something");
        tiContainers.setDgClass("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers.setGrossWeight(new BigDecimal("2.3"));
        tiContainers
                .setGrossWeightUnit("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers.setGuid(UUID.randomUUID());
        tiContainers.setId(1L);
        tiContainers.setIsDeleted(true);
        tiContainers.setNetWeight(new BigDecimal("2.3"));
        tiContainers
                .setNetWeightUnit("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers.setNoOfPackages("java.text");
        tiContainers.setNumber("42");
        tiContainers
                .setSubstanceName("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers.setTenantId(1);
        tiContainers.setTiLegId(1L);
        tiContainers.setTunnelRestrictionCode(
                "Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers.setType("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers.setUnNumber("42");
        tiContainers.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers.setUpdatedBy("2020-03-01");
        tiContainers.setVolume(new BigDecimal("2.3"));
        tiContainers.setVolumeUnit("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");

        TiContainers tiContainers2 = new TiContainers();
        tiContainers2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers2.setCreatedBy("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers2.setDangerous(false);
        tiContainers2
                .setDescription("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers2.setDgClass("request_id");
        tiContainers2.setGrossWeight(new BigDecimal("2.3"));
        tiContainers2.setGrossWeightUnit("request_id");
        tiContainers2.setGuid(UUID.randomUUID());
        tiContainers2.setId(2L);
        tiContainers2.setIsDeleted(false);
        tiContainers2.setNetWeight(new BigDecimal("2.3"));
        tiContainers2.setNetWeightUnit("request_id");
        tiContainers2
                .setNoOfPackages("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers2.setNumber("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers2.setSubstanceName("request_id");
        tiContainers2.setTenantId(2);
        tiContainers2.setTiLegId(2L);
        tiContainers2.setTunnelRestrictionCode("request_id");
        tiContainers2.setType("request_id");
        tiContainers2.setUnNumber("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiContainers2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers2.setUpdatedBy("2020/03/01");
        tiContainers2.setVolume(new BigDecimal("2.3"));
        tiContainers2.setVolumeUnit("request_id");

        ArrayList<TiContainers> content = new ArrayList<>();
        content.add(tiContainers2);
        content.add(tiContainers);
        PageImpl<TiContainers> pageImpl = new PageImpl<>(content);
        when(iTiContainerDao.findAll(Mockito.<Specification<TiContainers>>any(), Mockito.<Pageable>any()))
                .thenReturn(pageImpl);
        Runnable mockRunnable = mock(Runnable.class);
        when(masterDataUtils.withMdc(any(Runnable.class))).thenAnswer(invocation -> {
            Runnable argument = invocation.getArgument(0);
            argument.run();
            return mockRunnable;
        });
        TransportInstructionLegsContainersListResponse actualListResult = transportInstructionLegsContainersServiceImpl
                .list(new ListCommonRequest(), true);
        verify(iTiContainerDao).findAll(Mockito.<Specification<TiContainers>>any(), Mockito.<Pageable>any());
        verify(jsonHelper, atLeast(1)).convertValue(Mockito.<TiContainers>any(),
                Mockito.<Class<TransportInstructionLegsContainersResponse>>any());
        assertEquals(1, actualListResult.getTotalPages().intValue());
        assertEquals(2, actualListResult.getTiLegsContainersResponses().size());
        assertEquals(2L, actualListResult.getTotalCount().longValue());
    }

    /**
     * Method under test:
     * {@link ITransportInstructionLegsContainersService#list(ListCommonRequest, boolean)}
     */
    @Test
    void testList4() {
        ArrayList<TiContainers> content = new ArrayList<>();
        when(iTiContainerDao.findAll(Mockito.<Specification<TiContainers>>any(), Mockito.<Pageable>any()))
                .thenReturn(new PageImpl<>(content));
        ListCommonRequest.ListCommonRequestBuilder entityIdResult = ListCommonRequest.builder().entityId("42");
        ListCommonRequest.ListCommonRequestBuilder filterCriteriaResult = entityIdResult.filterCriteria(new ArrayList<>());
        ListCommonRequest.ListCommonRequestBuilder includeColumnsResult = filterCriteriaResult
                .includeColumns(new ArrayList<>());
        ListCommonRequest.ListCommonRequestBuilder populateRAKCResult = includeColumnsResult.includeTbls(new ArrayList<>())
                .notificationFlag(true)
                .populateRAKC(true);
        SortRequest sortRequest = SortRequest.builder().fieldName("Field Name").order("Order").build();
        ListCommonRequest request = populateRAKCResult.sortRequest(sortRequest).build();
        TransportInstructionLegsContainersListResponse actualListResult = transportInstructionLegsContainersServiceImpl
                .list(request, true);
        verify(iTiContainerDao).findAll(Mockito.<Specification<TiContainers>>any(), Mockito.<Pageable>any());
        assertEquals(0L, actualListResult.getTotalCount().longValue());
        assertEquals(1, actualListResult.getTotalPages().intValue());
        assertEquals(content, actualListResult.getTiLegsContainersResponses());
    }

    /**
     * Method under test:
     * {@link ITransportInstructionLegsContainersService#list(ListCommonRequest, boolean)}
     */
    @Test
    void testList5() {
        when(jsonHelper.convertValue(Mockito.<TiContainers>any(),
                Mockito.<Class<TransportInstructionLegsContainersResponse>>any())).thenThrow(new ValidationException("Msg"));

        TiContainers tiContainers = new TiContainers();
        tiContainers.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiContainers.setDangerous(true);
        tiContainers.setDescription("The characteristics of someone or something");
        tiContainers.setDgClass("DESC");
        tiContainers.setGrossWeight(new BigDecimal("2.3"));
        tiContainers.setGrossWeightUnit("DESC");
        tiContainers.setGuid(UUID.randomUUID());
        tiContainers.setId(1L);
        tiContainers.setIsDeleted(true);
        tiContainers.setNetWeight(new BigDecimal("2.3"));
        tiContainers.setNetWeightUnit("DESC");
        tiContainers.setNoOfPackages("java.text");
        tiContainers.setNumber("42");
        tiContainers.setSubstanceName("DESC");
        tiContainers.setTenantId(1);
        tiContainers.setTiLegId(1L);
        tiContainers.setTunnelRestrictionCode("DESC");
        tiContainers.setType("DESC");
        tiContainers.setUnNumber("42");
        tiContainers.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers.setUpdatedBy("2020-03-01");
        tiContainers.setVolume(new BigDecimal("2.3"));
        tiContainers.setVolumeUnit("DESC");

        ArrayList<TiContainers> content = new ArrayList<>();
        content.add(tiContainers);
        PageImpl<TiContainers> pageImpl = new PageImpl<>(content);
        when(iTiContainerDao.findAll(Mockito.<Specification<TiContainers>>any(), Mockito.<Pageable>any()))
                .thenReturn(pageImpl);
        ListCommonRequest.ListCommonRequestBuilder entityIdResult = ListCommonRequest.builder().entityId("42");
        ListCommonRequest.ListCommonRequestBuilder filterCriteriaResult = entityIdResult.filterCriteria(new ArrayList<>());
        ListCommonRequest.ListCommonRequestBuilder includeColumnsResult = filterCriteriaResult
                .includeColumns(new ArrayList<>());
        ListCommonRequest.ListCommonRequestBuilder populateRAKCResult = includeColumnsResult.includeTbls(new ArrayList<>())
                .notificationFlag(true)
                .populateRAKC(true);
        SortRequest sortRequest = SortRequest.builder().fieldName("Field Name").order("Order").build();
        ListCommonRequest request = populateRAKCResult.sortRequest(sortRequest).build();
        assertThrows(ValidationException.class, () -> transportInstructionLegsContainersServiceImpl.list(request, true));
        verify(iTiContainerDao).findAll(Mockito.<Specification<TiContainers>>any(), Mockito.<Pageable>any());
        verify(jsonHelper).convertValue(Mockito.<TiContainers>any(),
                Mockito.<Class<TransportInstructionLegsContainersResponse>>any());
    }

    /**
     * Method under test:
     * {@link TransportInstructionLegsContainersServiceImpl#delete(Long)}
     */
    @Test
    void testDelete() {
        TiContainers tiContainers = new TiContainers();
        tiContainers.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiContainers.setDangerous(true);
        tiContainers.setDescription("The characteristics of someone or something");
        tiContainers.setDgClass("Dg Class");
        tiContainers.setGrossWeight(new BigDecimal("2.3"));
        tiContainers.setGrossWeightUnit("Gross Weight Unit");
        tiContainers.setGuid(UUID.randomUUID());
        tiContainers.setId(1L);
        tiContainers.setIsDeleted(true);
        tiContainers.setNetWeight(new BigDecimal("2.3"));
        tiContainers.setNetWeightUnit("Net Weight Unit");
        tiContainers.setNoOfPackages("java.text");
        tiContainers.setNumber("42");
        tiContainers.setSubstanceName("Substance Name");
        tiContainers.setTenantId(1);
        tiContainers.setTiLegId(1L);
        tiContainers.setTunnelRestrictionCode("Tunnel Restriction Code");
        tiContainers.setType("Type");
        tiContainers.setUnNumber("42");
        tiContainers.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers.setUpdatedBy("2020-03-01");
        tiContainers.setVolume(new BigDecimal("2.3"));
        tiContainers.setVolumeUnit("Volume Unit");
        Optional<TiContainers> ofResult = Optional.of(tiContainers);
        when(iTiContainerDao.findById(Mockito.<Long>any())).thenReturn(ofResult);
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenThrow(new ValidationException("Msg"));
        assertThrows(ValidationException.class, () -> transportInstructionLegsContainersServiceImpl.delete(1L));
        verify(iTiContainerDao).findById(Mockito.<Long>any());
        verify(iTiLegRepository).findById(Mockito.<Long>any());
    }

    /**
     * Method under test:
     * {@link TransportInstructionLegsContainersServiceImpl#delete(Long)}
     */
    @Test
    void testDelete2() {
        Optional<TiContainers> emptyResult = Optional.empty();
        when(iTiContainerDao.findById(Mockito.<Long>any())).thenReturn(emptyResult);
        assertThrows(ValidationException.class, () -> transportInstructionLegsContainersServiceImpl.delete(1L));
        verify(iTiContainerDao).findById(Mockito.<Long>any());
    }

    /**
     * Method under test:
     * {@link TransportInstructionLegsContainersServiceImpl#retrieveById(Long)}
     */
    @Test
    void testRetrieveById() {
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder dgClassResult = TransportInstructionLegsContainersResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something")
                .dgClass("Dg Class");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsContainersResponse.TransportInstructionLegsContainersResponseBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .number("42")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .type("Type")
                .unNumber("42");
        TransportInstructionLegsContainersResponse buildResult = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit")
                .build();
        when(jsonHelper.convertValue(Mockito.<TiContainers>any(),
                Mockito.<Class<TransportInstructionLegsContainersResponse>>any())).thenReturn(buildResult);

        TiContainers tiContainers = new TiContainers();
        tiContainers.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiContainers.setDangerous(true);
        tiContainers.setDescription("The characteristics of someone or something");
        tiContainers.setDgClass("Dg Class");
        tiContainers.setGrossWeight(new BigDecimal("2.3"));
        tiContainers.setGrossWeightUnit("Gross Weight Unit");
        tiContainers.setGuid(UUID.randomUUID());
        tiContainers.setId(1L);
        tiContainers.setIsDeleted(true);
        tiContainers.setNetWeight(new BigDecimal("2.3"));
        tiContainers.setNetWeightUnit("Net Weight Unit");
        tiContainers.setNoOfPackages("java.text");
        tiContainers.setNumber("42");
        tiContainers.setSubstanceName("Substance Name");
        tiContainers.setTenantId(1);
        tiContainers.setTiLegId(1L);
        tiContainers.setTunnelRestrictionCode("Tunnel Restriction Code");
        tiContainers.setType("Type");
        tiContainers.setUnNumber("42");
        tiContainers.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers.setUpdatedBy("2020-03-01");
        tiContainers.setVolume(new BigDecimal("2.3"));
        tiContainers.setVolumeUnit("Volume Unit");
        Optional<TiContainers> ofResult = Optional.of(tiContainers);
        when(iTiContainerDao.findById(Mockito.<Long>any())).thenReturn(ofResult);
        transportInstructionLegsContainersServiceImpl.retrieveById(1L);
        verify(iTiContainerDao).findById(Mockito.<Long>any());
        verify(jsonHelper).convertValue(Mockito.<TiContainers>any(),
                Mockito.<Class<TransportInstructionLegsContainersResponse>>any());
    }

    @Test
    void testDelete3() throws RunnerException, JsonProcessingException, IllegalAccessException, NoSuchFieldException,
            NoSuchMethodException, InvocationTargetException {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        TiLegs tiLegs = new TiLegs();
        tiLegs.setPickupDeliveryDetailsId(1l);
        when(iTiLegRepository.findById(any())).thenReturn(Optional.of(tiLegs));
        when(iTiContainerDao.findById(Mockito.<Long>any())).thenReturn(Optional.of(new TiContainers()));
        doNothing().when(iTiContainerDao).delete(any());
        transportInstructionLegsContainersServiceImpl.delete(1L);
        verify(iTiContainerDao).findById(Mockito.<Long>any());
    }

    /**
     * Method under test:
     * {@link TransportInstructionLegsContainersServiceImpl#retrieveById(Long)}
     */
    @Test
    void testRetrieveById2() {
        when(jsonHelper.convertValue(Mockito.<TiContainers>any(),
                Mockito.<Class<TransportInstructionLegsContainersResponse>>any())).thenThrow(new ValidationException("Msg"));

        TiContainers tiContainers = new TiContainers();
        tiContainers.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiContainers.setDangerous(true);
        tiContainers.setDescription("The characteristics of someone or something");
        tiContainers.setDgClass("Dg Class");
        tiContainers.setGrossWeight(new BigDecimal("2.3"));
        tiContainers.setGrossWeightUnit("Gross Weight Unit");
        tiContainers.setGuid(UUID.randomUUID());
        tiContainers.setId(1L);
        tiContainers.setIsDeleted(true);
        tiContainers.setNetWeight(new BigDecimal("2.3"));
        tiContainers.setNetWeightUnit("Net Weight Unit");
        tiContainers.setNoOfPackages("java.text");
        tiContainers.setNumber("42");
        tiContainers.setSubstanceName("Substance Name");
        tiContainers.setTenantId(1);
        tiContainers.setTiLegId(1L);
        tiContainers.setTunnelRestrictionCode("Tunnel Restriction Code");
        tiContainers.setType("Type");
        tiContainers.setUnNumber("42");
        tiContainers.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiContainers.setUpdatedBy("2020-03-01");
        tiContainers.setVolume(new BigDecimal("2.3"));
        tiContainers.setVolumeUnit("Volume Unit");
        Optional<TiContainers> ofResult = Optional.of(tiContainers);
        when(iTiContainerDao.findById(Mockito.<Long>any())).thenReturn(ofResult);
        assertThrows(ValidationException.class, () -> transportInstructionLegsContainersServiceImpl.retrieveById(1L));
        verify(iTiContainerDao).findById(Mockito.<Long>any());
        verify(jsonHelper).convertValue(Mockito.<TiContainers>any(),
                Mockito.<Class<TransportInstructionLegsContainersResponse>>any());
    }
}
