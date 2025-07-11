package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.enums.TILegType;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ITiPackageDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsPackagesListRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsPackagesRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsPackagesListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsPackagesResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.entity.TiPackages;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ITiLegRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {TransportInstructionLegsPackagesServiceImpl.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
class TransportInstructionLegsPackagesServiceImplTest {
    @MockBean
    private DependentServiceHelper dependentServiceHelper;

    @MockBean
    private IAuditLogService iAuditLogService;


    @MockBean
    private ITiPackageDao iTiPackageDao;

    @MockBean
    private ITiLegRepository iTiLegRepository;

    @MockBean
    private JsonHelper jsonHelper;

    @MockBean
    private UserContext userContext;

    @Autowired
    private TransportInstructionLegsPackagesServiceImpl transportInstructionLegsPackagesService;


    @Test
    void testCreate() {

        TiPackages tiPackages = new TiPackages();
        tiPackages.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiPackages.setDangerous(true);
        tiPackages.setDescription("The characteristics of someone or something");
        tiPackages.setGrossWeight(new BigDecimal("2.3"));
        tiPackages.setGrossWeightUnit("Gross Weight Unit");
        tiPackages.setGuid(UUID.randomUUID());
        tiPackages.setId(1L);
        tiPackages.setIsDeleted(true);
        tiPackages.setNetWeight(new BigDecimal("2.3"));
        tiPackages.setNetWeightUnit("Net Weight Unit");
        tiPackages.setNoOfPackages("java.text");
        tiPackages.setSubstanceName("Substance Name");
        tiPackages.setTenantId(1);
        tiPackages.setTiLegId(1L);
        tiPackages.setTunnelRestrictionCode("Tunnel Restriction Code");
        tiPackages.setPackageType("BBL");
        tiPackages.setUnNumber("42");
        tiPackages.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages.setUpdatedBy("2020-03-01");
        tiPackages.setVolume(new BigDecimal("2.3"));
        tiPackages.setVolumeUnit("Volume Unit");
        when(jsonHelper.convertValue(Mockito.<TransportInstructionLegsPackagesRequest>any(),
                Mockito.<Class<TiPackages>>any())).thenReturn(tiPackages);
        when(iTiPackageDao.save(Mockito.<TiPackages>any())).thenThrow(new ValidationException("request_id"));

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
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiReferences(new ArrayList<>());
        tiLegs.setTiTruckDriverDetails(new ArrayList<>());
        tiLegs.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setUpdatedBy("2020-03-01");
        Optional<TiLegs> ofResult = Optional.of(tiLegs);
        TransportInstructionLegsPackagesRequest transportInstructionLegsPackagesRequest = new TransportInstructionLegsPackagesRequest();
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        assertThrows(ValidationException.class,
                () -> transportInstructionLegsPackagesService.create(transportInstructionLegsPackagesRequest));
        verify(iTiPackageDao).save(Mockito.<TiPackages>any());
        verify(jsonHelper).convertValue(Mockito.<TransportInstructionLegsPackagesRequest>any(),
                Mockito.<Class<TiPackages>>any());
        verify(iTiLegRepository).findById(Mockito.<Long>any());
    }

    @Test
    void testCreate2() {
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
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiReferences(new ArrayList<>());
        tiLegs.setTiTruckDriverDetails(new ArrayList<>());
        tiLegs.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setUpdatedBy("2020-03-01");
        Optional<TiLegs> ofResult = Optional.of(tiLegs);
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder dgClassResult = TransportInstructionLegsPackagesRequest
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something");
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .packageType("BBL")
                .unNumber("42");
        TransportInstructionLegsPackagesRequest request = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit")
                .build();
        assertThrows(ValidationException.class, () -> transportInstructionLegsPackagesService.create(request));
        verify(iTiLegRepository).findById(Mockito.<Long>any());
    }


    @Test
    void testCreate4() throws RunnerException, JsonProcessingException, IllegalAccessException, NoSuchFieldException,
            NoSuchMethodException, InvocationTargetException {
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
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiReferences(new ArrayList<>());
        tiLegs.setTiTruckDriverDetails(new ArrayList<>());
        tiLegs.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setUpdatedBy("2020-03-01");
        Optional<TiLegs> ofResult = Optional.of(tiLegs);
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder dgClassResult = TransportInstructionLegsPackagesRequest
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something");
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("KG");
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("KG")
                .noOfPackages("20")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .packageType("BBL")
                .length(BigDecimal.TEN)
                .width(BigDecimal.TEN)
                .height(BigDecimal.TEN)
                .widthUnit("FT")
                .heightUnit("FT")
                .lengthUnit("FT")
                .unNumber("42");
        TransportInstructionLegsPackagesRequest request = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("M3")
                .build();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsPackagesResponse.class))).thenReturn(new TransportInstructionLegsPackagesResponse());
        when(iTiPackageDao.save(any())).thenReturn(new TiPackages());
        when(jsonHelper.convertValue(request, TiPackages.class)).thenReturn(new TiPackages());
        TransportInstructionLegsPackagesResponse response = transportInstructionLegsPackagesService.create(request);
        assertNotNull(response);
        verify(iTiLegRepository).findById(Mockito.<Long>any());
    }
    @Test
    void testCreateWithValidationException() {

        Optional<TiLegs> ofResult = Optional.of(new TiLegs());
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder dgClassResult = TransportInstructionLegsPackagesRequest
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something");
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("KG");
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("KG")
                .noOfPackages("20")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .packageType("BBL")
                .length(BigDecimal.TEN)
                .lengthUnit("FT")
                .unNumber("42");
        TransportInstructionLegsPackagesRequest request = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("M3")
                .build();

       assertThrows(ValidationException.class,() -> transportInstructionLegsPackagesService.create(request));
    }
    @Test
    void testCreateWithValidationException1() {

        Optional<TiLegs> ofResult = Optional.of(new TiLegs());
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder dgClassResult = TransportInstructionLegsPackagesRequest
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something");
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("KG");
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("KG")
                .noOfPackages("20")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .packageType("BBL")
                .length(BigDecimal.TEN)
                .width(BigDecimal.TEN)
                .height(BigDecimal.TEN)
                .lengthUnit("FT")
                .unNumber("42");
        TransportInstructionLegsPackagesRequest request = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("M3")
                .build();

        assertThrows(ValidationException.class,() -> transportInstructionLegsPackagesService.create(request));
    }
    @Test
    void testCreateBulk() throws RunnerException, JsonProcessingException, IllegalAccessException, NoSuchFieldException,
            NoSuchMethodException, InvocationTargetException {
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
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiReferences(new ArrayList<>());
        tiLegs.setTiTruckDriverDetails(new ArrayList<>());
        tiLegs.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setUpdatedBy("2020-03-01");
        Optional<TiLegs> ofResult = Optional.of(tiLegs);
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder dgClassResult = TransportInstructionLegsPackagesRequest
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something");
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("KG");
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("KG")
                .noOfPackages("20")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .packageType("BBL")
                .length(BigDecimal.TEN)
                .width(BigDecimal.TEN)
                .height(BigDecimal.TEN)
                .widthUnit("FT")
                .heightUnit("FT")
                .lengthUnit("FT")
                .unNumber("42");
        TransportInstructionLegsPackagesRequest request = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("M3")
                .build();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsPackagesResponse.class))).thenReturn(new TransportInstructionLegsPackagesResponse());
        when(iTiPackageDao.saveAll(any())).thenReturn(List.of(new TiPackages()));
        when(jsonHelper.convertValue(request, TiPackages.class)).thenReturn(new TiPackages());
        TransportInstructionLegsPackagesListRequest packagesListRequest = new TransportInstructionLegsPackagesListRequest();
        packagesListRequest.setPackagesRequests(List.of(request));
        TransportInstructionLegsPackagesListResponse response = transportInstructionLegsPackagesService.bulkCreate(packagesListRequest);
        assertNotNull(response);
        verify(iTiLegRepository).findById(Mockito.<Long>any());
    }
    @Test
    void testUpdate() throws RunnerException, JsonProcessingException, IllegalAccessException, NoSuchFieldException,
            NoSuchMethodException, InvocationTargetException {

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
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiReferences(new ArrayList<>());
        tiLegs.setTiTruckDriverDetails(new ArrayList<>());
        tiLegs.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setUpdatedBy("2020-03-01");
        Optional<TiLegs> ofResult = Optional.of(tiLegs);
        TiPackages tiPackages = new TiPackages();
        tiPackages.setId(1L);
        when(iTiPackageDao.findById(1L)).thenReturn(Optional.of(tiPackages));
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder dgClassResult = TransportInstructionLegsPackagesRequest
                .builder()
                .tiLegId(1L)
                .dangerous(true)
                .description("The characteristics of someone or something");
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("KG");
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsPackagesRequest.TransportInstructionLegsPackagesRequestBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("KG")
                .noOfPackages("20")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .packageType("BBL")
                .unNumber("42");
        TransportInstructionLegsPackagesRequest request = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("M3")
                .build();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsPackagesResponse.class))).thenReturn(new TransportInstructionLegsPackagesResponse());
        when(iTiPackageDao.save(any())).thenReturn(new TiPackages());
        when(jsonHelper.convertValue(request, TiPackages.class)).thenReturn(new TiPackages());
        TransportInstructionLegsPackagesResponse response = transportInstructionLegsPackagesService.update(request);
        assertNotNull(response);
        verify(iTiLegRepository).findById(Mockito.<Long>any());
    }

    @Test
    void testList() {
        ArrayList<TiPackages> content = new ArrayList<>();
        when(iTiPackageDao.findAll(Mockito.<Specification<TiPackages>>any(), Mockito.<Pageable>any()))
                .thenReturn(new PageImpl<>(content));
        TransportInstructionLegsPackagesListResponse actualListResult = transportInstructionLegsPackagesService
                .list(new ListCommonRequest());
        verify(iTiPackageDao).findAll(Mockito.<Specification<TiPackages>>any(), Mockito.<Pageable>any());
        assertEquals(0L, actualListResult.getTotalCount().longValue());
        assertEquals(1, actualListResult.getTotalPages().intValue());
        assertEquals(content, actualListResult.getTiLegsPackagesResponses());
    }

    @Test
    void testList2() {
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder dgClassResult = TransportInstructionLegsPackagesResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .packageType("BBL")
                .unNumber("42");
        TransportInstructionLegsPackagesResponse buildResult = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit")
                .build();
        when(jsonHelper.convertValue(Mockito.<TiPackages>any(),
                Mockito.<Class<TransportInstructionLegsPackagesResponse>>any())).thenReturn(buildResult);

        TiPackages tiPackages = new TiPackages();
        tiPackages.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiPackages.setDangerous(true);
        tiPackages.setDescription("The characteristics of someone or something");
        tiPackages.setGrossWeight(new BigDecimal("2.3"));
        tiPackages
                .setGrossWeightUnit("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiPackages.setGuid(UUID.randomUUID());
        tiPackages.setId(1L);
        tiPackages.setIsDeleted(true);
        tiPackages.setNetWeight(new BigDecimal("2.3"));
        tiPackages
                .setNetWeightUnit("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiPackages.setNoOfPackages("java.text");
        tiPackages
                .setSubstanceName("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiPackages.setTenantId(1);
        tiPackages.setTiLegId(1L);
        tiPackages.setTunnelRestrictionCode(
                "Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiPackages.setPackageType("BBL");
        tiPackages.setUnNumber("42");
        tiPackages.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages.setUpdatedBy("2020-03-01");
        tiPackages.setVolume(new BigDecimal("2.3"));
        tiPackages.setVolumeUnit("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");

        ArrayList<TiPackages> content = new ArrayList<>();
        content.add(tiPackages);
        PageImpl<TiPackages> pageImpl = new PageImpl<>(content);
        when(iTiPackageDao.findAll(Mockito.<Specification<TiPackages>>any(), Mockito.<Pageable>any()))
                .thenReturn(pageImpl);
        TransportInstructionLegsPackagesListResponse actualListResult = transportInstructionLegsPackagesService
                .list(new ListCommonRequest());
        verify(iTiPackageDao).findAll(Mockito.<Specification<TiPackages>>any(), Mockito.<Pageable>any());
        verify(jsonHelper).convertValue(Mockito.<TiPackages>any(),
                Mockito.<Class<TransportInstructionLegsPackagesResponse>>any());
        assertEquals(1, actualListResult.getTotalPages().intValue());
        assertEquals(1, actualListResult.getTiLegsPackagesResponses().size());
        assertEquals(1L, actualListResult.getTotalCount().longValue());
    }

    @Test
    void testList3() {
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder dgClassResult = TransportInstructionLegsPackagesResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .packageType("BBL")
                .unNumber("42");
        TransportInstructionLegsPackagesResponse buildResult = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit")
                .build();
        when(jsonHelper.convertValue(Mockito.<TiPackages>any(),
                Mockito.<Class<TransportInstructionLegsPackagesResponse>>any())).thenReturn(buildResult);

        TiPackages tiPackages = new TiPackages();
        tiPackages.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiPackages.setDangerous(true);
        tiPackages.setDescription("The characteristics of someone or something");
        tiPackages.setGrossWeight(new BigDecimal("2.3"));
        tiPackages
                .setGrossWeightUnit("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiPackages.setGuid(UUID.randomUUID());
        tiPackages.setId(1L);
        tiPackages.setIsDeleted(true);
        tiPackages.setNetWeight(new BigDecimal("2.3"));
        tiPackages
                .setNetWeightUnit("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiPackages.setNoOfPackages("java.text");
        tiPackages
                .setSubstanceName("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiPackages.setTenantId(1);
        tiPackages.setTiLegId(1L);
        tiPackages.setTunnelRestrictionCode(
                "Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiPackages.setPackageType("BBL");
        tiPackages.setUnNumber("42");
        tiPackages.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages.setUpdatedBy("2020-03-01");
        tiPackages.setVolume(new BigDecimal("2.3"));
        tiPackages.setVolumeUnit("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");

        TiPackages tiPackages2 = new TiPackages();
        tiPackages2.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages2.setCreatedBy("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiPackages2.setDangerous(false);
        tiPackages2
                .setDescription("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiPackages2.setGrossWeight(new BigDecimal("2.3"));
        tiPackages2.setGrossWeightUnit("request_id");
        tiPackages2.setGuid(UUID.randomUUID());
        tiPackages2.setId(2L);
        tiPackages2.setIsDeleted(false);
        tiPackages2.setNetWeight(new BigDecimal("2.3"));
        tiPackages2.setNetWeightUnit("request_id");
        tiPackages2
                .setNoOfPackages("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiPackages2.setSubstanceName("request_id");
        tiPackages2.setTenantId(2);
        tiPackages2.setTiLegId(2L);
        tiPackages2.setTunnelRestrictionCode("request_id");
        tiPackages2.setPackageType("request_id");
        tiPackages2.setUnNumber("Transport Instruction Legs containers list retrieved successfully for Request Id {} ");
        tiPackages2.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages2.setUpdatedBy("2020/03/01");
        tiPackages2.setVolume(new BigDecimal("2.3"));
        tiPackages2.setVolumeUnit("request_id");

        ArrayList<TiPackages> content = new ArrayList<>();
        content.add(tiPackages2);
        content.add(tiPackages);
        PageImpl<TiPackages> pageImpl = new PageImpl<>(content);
        when(iTiPackageDao.findAll(Mockito.<Specification<TiPackages>>any(), Mockito.<Pageable>any()))
                .thenReturn(pageImpl);
        TransportInstructionLegsPackagesListResponse actualListResult = transportInstructionLegsPackagesService
                .list(new ListCommonRequest());
        verify(iTiPackageDao).findAll(Mockito.<Specification<TiPackages>>any(), Mockito.<Pageable>any());
        verify(jsonHelper, atLeast(1)).convertValue(Mockito.<TiPackages>any(),
                Mockito.<Class<TransportInstructionLegsPackagesResponse>>any());
        assertEquals(1, actualListResult.getTotalPages().intValue());
        assertEquals(2, actualListResult.getTiLegsPackagesResponses().size());
        assertEquals(2L, actualListResult.getTotalCount().longValue());
    }

    @Test
    void testList4() {
        ArrayList<TiPackages> content = new ArrayList<>();
        when(iTiPackageDao.findAll(Mockito.<Specification<TiPackages>>any(), Mockito.<Pageable>any()))
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
        TransportInstructionLegsPackagesListResponse actualListResult = transportInstructionLegsPackagesService
                .list(request);
        verify(iTiPackageDao).findAll(Mockito.<Specification<TiPackages>>any(), Mockito.<Pageable>any());
        assertEquals(0L, actualListResult.getTotalCount().longValue());
        assertEquals(1, actualListResult.getTotalPages().intValue());
        assertEquals(content, actualListResult.getTiLegsPackagesResponses());
    }

    @Test
    void testList5() {
        when(jsonHelper.convertValue(Mockito.<TiPackages>any(),
                Mockito.<Class<TransportInstructionLegsPackagesResponse>>any())).thenThrow(new ValidationException("Msg"));

        TiPackages tiPackages = new TiPackages();
        tiPackages.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiPackages.setDangerous(true);
        tiPackages.setDescription("The characteristics of someone or something");
        tiPackages.setGrossWeight(new BigDecimal("2.3"));
        tiPackages.setGrossWeightUnit("DESC");
        tiPackages.setGuid(UUID.randomUUID());
        tiPackages.setId(1L);
        tiPackages.setIsDeleted(true);
        tiPackages.setNetWeight(new BigDecimal("2.3"));
        tiPackages.setNetWeightUnit("DESC");
        tiPackages.setNoOfPackages("java.text");
        tiPackages.setSubstanceName("DESC");
        tiPackages.setTenantId(1);
        tiPackages.setTiLegId(1L);
        tiPackages.setTunnelRestrictionCode("DESC");
        tiPackages.setPackageType("BBL");
        tiPackages.setUnNumber("42");
        tiPackages.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages.setUpdatedBy("2020-03-01");
        tiPackages.setVolume(new BigDecimal("2.3"));
        tiPackages.setVolumeUnit("DESC");

        ArrayList<TiPackages> content = new ArrayList<>();
        content.add(tiPackages);
        PageImpl<TiPackages> pageImpl = new PageImpl<>(content);
        when(iTiPackageDao.findAll(Mockito.<Specification<TiPackages>>any(), Mockito.<Pageable>any()))
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
        assertThrows(ValidationException.class, () -> transportInstructionLegsPackagesService.list(request));
        verify(iTiPackageDao).findAll(Mockito.<Specification<TiPackages>>any(), Mockito.<Pageable>any());
        verify(jsonHelper).convertValue(Mockito.<TiPackages>any(),
                Mockito.<Class<TransportInstructionLegsPackagesResponse>>any());
    }

    @Test
    void testDelete() {
        TiPackages tiPackages = new TiPackages();
        tiPackages.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiPackages.setDangerous(true);
        tiPackages.setDescription("The characteristics of someone or something");
        tiPackages.setGrossWeight(new BigDecimal("2.3"));
        tiPackages.setGrossWeightUnit("Gross Weight Unit");
        tiPackages.setGuid(UUID.randomUUID());
        tiPackages.setId(1L);
        tiPackages.setIsDeleted(true);
        tiPackages.setNetWeight(new BigDecimal("2.3"));
        tiPackages.setNetWeightUnit("Net Weight Unit");
        tiPackages.setNoOfPackages("java.text");
        tiPackages.setSubstanceName("Substance Name");
        tiPackages.setTenantId(1);
        tiPackages.setTiLegId(1L);
        tiPackages.setTunnelRestrictionCode("Tunnel Restriction Code");
        tiPackages.setPackageType("BBL");
        tiPackages.setUnNumber("42");
        tiPackages.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages.setUpdatedBy("2020-03-01");
        tiPackages.setVolume(new BigDecimal("2.3"));
        tiPackages.setVolumeUnit("Volume Unit");
        Optional<TiPackages> ofResult = Optional.of(tiPackages);
        when(iTiPackageDao.findById(Mockito.<Long>any())).thenReturn(ofResult);
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenThrow(new ValidationException("Msg"));
        assertThrows(ValidationException.class, () -> transportInstructionLegsPackagesService.delete(1L));
        verify(iTiPackageDao).findById(Mockito.<Long>any());
        verify(iTiLegRepository).findById(Mockito.<Long>any());
    }

    @Test
    void testDelete2() {
        Optional<TiPackages> emptyResult = Optional.empty();
        when(iTiPackageDao.findById(Mockito.<Long>any())).thenReturn(emptyResult);
        assertThrows(ValidationException.class, () -> transportInstructionLegsPackagesService.delete(1L));
        verify(iTiPackageDao).findById(Mockito.<Long>any());
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
        when(iTiPackageDao.findById(Mockito.<Long>any())).thenReturn(Optional.of(new TiPackages()));
        doNothing().when(iTiPackageDao).delete(any());
        transportInstructionLegsPackagesService.delete(1L);
        verify(iTiPackageDao).findById(Mockito.<Long>any());
    }

    @Test
    void testRetrieveById() {
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder dgClassResult = TransportInstructionLegsPackagesResponse
                .builder()
                .dangerous(true)
                .description("The characteristics of someone or something");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder grossWeightUnitResult = dgClassResult
                .grossWeight(new BigDecimal("2.3"))
                .grossWeightUnit("Gross Weight Unit");
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder idResult = grossWeightUnitResult
                .guid(UUID.randomUUID())
                .id(1L);
        TransportInstructionLegsPackagesResponse.TransportInstructionLegsPackagesResponseBuilder unNumberResult = idResult
                .netWeight(new BigDecimal("2.3"))
                .netWeightUnit("Net Weight Unit")
                .noOfPackages("java.text")
                .substanceName("Substance Name")
                .tiLegId(1L)
                .tunnelRestrictionCode("Tunnel Restriction Code")
                .packageType("BBL")
                .unNumber("42");
        TransportInstructionLegsPackagesResponse buildResult = unNumberResult.volume(new BigDecimal("2.3"))
                .volumeUnit("Volume Unit")
                .build();
        when(jsonHelper.convertValue(Mockito.<TiPackages>any(),
                Mockito.<Class<TransportInstructionLegsPackagesResponse>>any())).thenReturn(buildResult);

        TiPackages tiPackages = new TiPackages();
        tiPackages.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiPackages.setDangerous(true);
        tiPackages.setDescription("The characteristics of someone or something");
        tiPackages.setGrossWeight(new BigDecimal("2.3"));
        tiPackages.setGrossWeightUnit("Gross Weight Unit");
        tiPackages.setGuid(UUID.randomUUID());
        tiPackages.setId(1L);
        tiPackages.setIsDeleted(true);
        tiPackages.setNetWeight(new BigDecimal("2.3"));
        tiPackages.setNetWeightUnit("Net Weight Unit");
        tiPackages.setNoOfPackages("java.text");
        tiPackages.setSubstanceName("Substance Name");
        tiPackages.setTenantId(1);
        tiPackages.setTiLegId(1L);
        tiPackages.setTunnelRestrictionCode("Tunnel Restriction Code");
        tiPackages.setPackageType("BBL");
        tiPackages.setUnNumber("42");
        tiPackages.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages.setUpdatedBy("2020-03-01");
        tiPackages.setVolume(new BigDecimal("2.3"));
        tiPackages.setVolumeUnit("Volume Unit");
        Optional<TiPackages> ofResult = Optional.of(tiPackages);
        when(iTiPackageDao.findById(Mockito.<Long>any())).thenReturn(ofResult);
        transportInstructionLegsPackagesService.retrieveById(1L);
        verify(iTiPackageDao).findById(Mockito.<Long>any());
        verify(jsonHelper).convertValue(Mockito.<TiPackages>any(),
                Mockito.<Class<TransportInstructionLegsPackagesResponse>>any());
    }


    @Test
    void testRetrieveById2() {
        when(jsonHelper.convertValue(Mockito.<TiPackages>any(),
                Mockito.<Class<TransportInstructionLegsPackagesResponse>>any())).thenThrow(new ValidationException("Msg"));

        TiPackages tiPackages = new TiPackages();
        tiPackages.setCreatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages.setCreatedBy("Jan 1, 2020 8:00am GMT+0100");
        tiPackages.setDangerous(true);
        tiPackages.setDescription("The characteristics of someone or something");
        tiPackages.setGrossWeight(new BigDecimal("2.3"));
        tiPackages.setGrossWeightUnit("Gross Weight Unit");
        tiPackages.setGuid(UUID.randomUUID());
        tiPackages.setId(1L);
        tiPackages.setIsDeleted(true);
        tiPackages.setNetWeight(new BigDecimal("2.3"));
        tiPackages.setNetWeightUnit("Net Weight Unit");
        tiPackages.setNoOfPackages("java.text");
        tiPackages.setSubstanceName("Substance Name");
        tiPackages.setTenantId(1);
        tiPackages.setTiLegId(1L);
        tiPackages.setTunnelRestrictionCode("Tunnel Restriction Code");
        tiPackages.setPackageType("BBL");
        tiPackages.setUnNumber("42");
        tiPackages.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiPackages.setUpdatedBy("2020-03-01");
        tiPackages.setVolume(new BigDecimal("2.3"));
        tiPackages.setVolumeUnit("Volume Unit");
        Optional<TiPackages> ofResult = Optional.of(tiPackages);
        when(iTiPackageDao.findById(Mockito.<Long>any())).thenReturn(ofResult);
        assertThrows(ValidationException.class, () -> transportInstructionLegsPackagesService.retrieveById(1L));
        verify(iTiPackageDao).findById(Mockito.<Long>any());
        verify(jsonHelper).convertValue(Mockito.<TiPackages>any(),
                Mockito.<Class<TransportInstructionLegsPackagesResponse>>any());
    }

    @Test
    void testCreate_shouldThrowException_whenTiLegsNotFound() {
        Long invalidTiLegId = 999L;
        TransportInstructionLegsPackagesRequest request = new TransportInstructionLegsPackagesRequest();
        request.setTiLegId(invalidTiLegId);
        when(iTiLegRepository.findById(invalidTiLegId)).thenReturn(Optional.empty());
        ValidationException ex = assertThrows(ValidationException.class, () ->
                transportInstructionLegsPackagesService.create(request)
        );
        assertEquals("Transport Instruction Legs does not exist for tiId: " + invalidTiLegId, ex.getMessage());
    }

    @Test
    void testUpdate_shouldThrowException_whenPackageNotFound() {
        Long invalidPackageId = 888L;
        Long validTiLegId = 100L;
        TransportInstructionLegsPackagesRequest request = new TransportInstructionLegsPackagesRequest();
        request.setId(invalidPackageId);
        request.setTiLegId(validTiLegId);
        when(iTiPackageDao.findById(invalidPackageId)).thenReturn(Optional.empty());
        ValidationException ex = assertThrows(ValidationException.class, () ->
                transportInstructionLegsPackagesService.update(request)
        );
        assertEquals("Invalid Transport Instruction Legs packages id" + invalidPackageId, ex.getMessage());
    }

    @Test
    void testUpdate_shouldThrowException_whenTiLegsNotFound() {
        Long packageId = 111L;
        Long invalidTiLegId = 999L;
        TransportInstructionLegsPackagesRequest request = new TransportInstructionLegsPackagesRequest();
        request.setId(packageId);
        request.setTiLegId(invalidTiLegId);
        TiPackages existingPackage = new TiPackages();
        when(iTiPackageDao.findById(packageId)).thenReturn(Optional.of(existingPackage));
        when(iTiLegRepository.findById(invalidTiLegId)).thenReturn(Optional.empty());
        ValidationException ex = assertThrows(ValidationException.class, () ->
                transportInstructionLegsPackagesService.update(request)
        );
        assertEquals("Transport Instruction Legs does not exist for tiId: " + invalidTiLegId, ex.getMessage());
    }

    @Test
    void testRetrieveById_shouldThrowException_whenPackageNotFound() {
        Long invalidPackageId = 123L;
        when(iTiPackageDao.findById(invalidPackageId)).thenReturn(Optional.empty());
        ValidationException ex = assertThrows(ValidationException.class, () ->
                transportInstructionLegsPackagesService.retrieveById(invalidPackageId)
        );
        assertEquals("Invalid Ti legs package Id: " + invalidPackageId, ex.getMessage());
    }

    @Test
    void testBulkCreate_shouldThrowException_whenTiLegIdMismatch() {
        Long legId1 = 100L, legId2 = 200L;
        TransportInstructionLegsPackagesRequest req1 = new TransportInstructionLegsPackagesRequest();
        req1.setTiLegId(legId1);
        TransportInstructionLegsPackagesRequest req2 = new TransportInstructionLegsPackagesRequest();
        req2.setTiLegId(legId2);
        TransportInstructionLegsPackagesListRequest request = new TransportInstructionLegsPackagesListRequest();
        request.setPackagesRequests(List.of(req1, req2));
        ValidationException ex = assertThrows(ValidationException.class, () ->
                transportInstructionLegsPackagesService.bulkCreate(request)
        );
        assertEquals("All tiLegId values must be the same", ex.getMessage());
    }

    @Test
    void testBulkCreate_shouldThrowException_whenTiLegNotFound() {
        Long invalidTiLegId = 555L;
        TransportInstructionLegsPackagesRequest req = new TransportInstructionLegsPackagesRequest();
        req.setTiLegId(invalidTiLegId);
        TransportInstructionLegsPackagesListRequest request = new TransportInstructionLegsPackagesListRequest();
        request.setPackagesRequests(List.of(req));
        when(iTiLegRepository.findById(invalidTiLegId)).thenReturn(Optional.empty());
        ValidationException ex = assertThrows(ValidationException.class, () ->
                transportInstructionLegsPackagesService.bulkCreate(request)
        );
        assertEquals("Transport Instruction Legs does not exist for tiId: " + invalidTiLegId, ex.getMessage());
    }
}
