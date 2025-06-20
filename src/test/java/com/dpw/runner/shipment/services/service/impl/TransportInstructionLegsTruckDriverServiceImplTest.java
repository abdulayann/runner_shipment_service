package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.enums.TILegType;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ITiTruckDriverDetailDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsTruckDriverRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsTruckDriverListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsTruckDriverResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.TiLegs;
import com.dpw.runner.shipment.services.entity.TiTruckDriverDetails;
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
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {TransportInstructionLegsTruckDriverServiceImpl.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
class TransportInstructionLegsTruckDriverServiceImplTest {
    @MockBean
    private DependentServiceHelper dependentServiceHelper;

    @MockBean
    private IAuditLogService iAuditLogService;

    @MockBean
    private ITiTruckDriverDetailDao iTiTruckDriverDetailDao;

    @MockBean
    private ITiLegRepository iTiLegRepository;

    @MockBean
    private JsonHelper jsonHelper;

    @MockBean
    private UserContext userContext;

    @Autowired
    private TransportInstructionLegsTruckDriverServiceImpl transportInstructionLegsTruckDriverService;

    @Test
    void testCreate() {
        ContainerNumberCheckResponse containerNumberCheckResponse = new ContainerNumberCheckResponse();
        containerNumberCheckResponse.setIsLastDigitCorrect(true);
        containerNumberCheckResponse.setLastDigit(1);
        containerNumberCheckResponse.setSuccess(true);

        when(iTiTruckDriverDetailDao.save(Mockito.<TiTruckDriverDetails>any())).thenThrow(new ValidationException("request_id"));

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
        TransportInstructionLegsTruckDriverRequest transportInstructionLegsTruckDriverRequest = new TransportInstructionLegsTruckDriverRequest();
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        assertThrows(ValidationException.class,
                () -> transportInstructionLegsTruckDriverService.create(transportInstructionLegsTruckDriverRequest));
        verify(iTiLegRepository).findById(Mockito.<Long>any());
    }

    @Test
    void testCreate2() throws RunnerException, JsonProcessingException, IllegalAccessException, NoSuchFieldException,
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
        tiLegs.setTiContainers(new ArrayList<>());
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiReferences(new ArrayList<>());
        tiLegs.setTiTruckDriverDetails(new ArrayList<>());
        tiLegs.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setUpdatedBy("2020-03-01");
        Optional<TiLegs> ofResult = Optional.of(tiLegs);
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        TransportInstructionLegsTruckDriverRequest request = TransportInstructionLegsTruckDriverRequest
                .builder()
                .tiLegId(1l)
                .driverName("Rajesh")
                .driverMobileNumber("912223334445")
                .trailerNumberPlate("TRK123")
                .truckNumberPlate("truck12")
                .truckOrTrailerType("SL20").build();

        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsTruckDriverResponse.class))).thenReturn(new TransportInstructionLegsTruckDriverResponse());
        when(jsonHelper.convertValue(any(), eq(TiTruckDriverDetails.class))).thenReturn(new TiTruckDriverDetails());
        when(iTiTruckDriverDetailDao.save(any())).thenReturn(new TiTruckDriverDetails());
        when(jsonHelper.convertValue(any(), eq(TiTruckDriverDetails.class))).thenReturn(new TiTruckDriverDetails());
        TransportInstructionLegsTruckDriverResponse response = transportInstructionLegsTruckDriverService.create(request);
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
        tiLegs.setTiContainers(new ArrayList<>());
        tiLegs.setTiPackages(new ArrayList<>());
        tiLegs.setTiReferences(new ArrayList<>());
        tiLegs.setTiTruckDriverDetails(new ArrayList<>());
        tiLegs.setUpdatedAt(LocalDate.of(1970, 1, 1).atStartOfDay());
        tiLegs.setUpdatedBy("2020-03-01");
        Optional<TiLegs> ofResult = Optional.of(tiLegs);
        when(iTiLegRepository.findById(Mockito.<Long>any())).thenReturn(ofResult);
        TiTruckDriverDetails truckDriverDetails = new TiTruckDriverDetails();
        when(iTiTruckDriverDetailDao.findById(any())).thenReturn(Optional.of(truckDriverDetails));
        TransportInstructionLegsTruckDriverRequest request = TransportInstructionLegsTruckDriverRequest
                .builder()
                .id(1l)
                .tiLegId(1l)
                .driverName("Rajesh")
                .driverMobileNumber("912223334445")
                .trailerNumberPlate("TRK123")
                .truckNumberPlate("truck12")
                .truckOrTrailerType("SL20").build();

        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        when(jsonHelper.convertValue(any(), eq(TransportInstructionLegsTruckDriverResponse.class))).thenReturn(new TransportInstructionLegsTruckDriverResponse());
        when(jsonHelper.convertValue(any(), eq(TiTruckDriverDetails.class))).thenReturn(new TiTruckDriverDetails());
        when(iTiTruckDriverDetailDao.save(any())).thenReturn(new TiTruckDriverDetails());
        when(jsonHelper.convertValue(any(), eq(TiTruckDriverDetails.class))).thenReturn(new TiTruckDriverDetails());
        TransportInstructionLegsTruckDriverResponse response = transportInstructionLegsTruckDriverService.update(request);
        assertNotNull(response);
        verify(iTiLegRepository).findById(Mockito.<Long>any());
    }

    @Test
    void testList() {
        ArrayList<TiTruckDriverDetails> content = new ArrayList<>();
        when(iTiTruckDriverDetailDao.findAll(Mockito.<Specification<TiTruckDriverDetails>>any(), Mockito.<Pageable>any()))
                .thenReturn(new PageImpl<>(content));
        TransportInstructionLegsTruckDriverListResponse actualListResult = transportInstructionLegsTruckDriverService
                .list(new ListCommonRequest());
        verify(iTiTruckDriverDetailDao).findAll(Mockito.<Specification<TiTruckDriverDetails>>any(), Mockito.<Pageable>any());
        assertEquals(0L, actualListResult.getTotalCount().longValue());
        assertEquals(1, actualListResult.getTotalPages().intValue());
        assertEquals(content, actualListResult.getTiLegsTruckDriverResponses());
    }

    /**
     * Method under test:
     * {@link TransportInstructionLegsContainersServiceImpl#list(ListCommonRequest)}
     */
    @Test
    void testList2() {
        TransportInstructionLegsTruckDriverResponse response = TransportInstructionLegsTruckDriverResponse
                .builder()
                .id(1l)
                .tiLegId(1l)
                .driverName("Rajesh")
                .driverMobileNumber("912223334445")
                .trailerNumberPlate("TRK123")
                .truckNumberPlate("truck12")
                .truckOrTrailerType("SL20").build();

        when(jsonHelper.convertValue(Mockito.<TiTruckDriverDetails>any(),
                Mockito.<Class<TransportInstructionLegsTruckDriverResponse>>any())).thenReturn(response);
        TiTruckDriverDetails truckDriverDetails = new TiTruckDriverDetails();
        truckDriverDetails.setTiLegId(1l);
        truckDriverDetails.setDriverName("Rajesh");
        truckDriverDetails.setDriverMobileNumber("912223334441");

        ArrayList<TiTruckDriverDetails> content = new ArrayList<>();
        content.add(truckDriverDetails);
        PageImpl<TiTruckDriverDetails> pageImpl = new PageImpl<>(content);
        when(iTiTruckDriverDetailDao.findAll(Mockito.<Specification<TiTruckDriverDetails>>any(), Mockito.<Pageable>any()))
                .thenReturn(pageImpl);
        TransportInstructionLegsTruckDriverListResponse actualListResult = transportInstructionLegsTruckDriverService
                .list(new ListCommonRequest());
        verify(iTiTruckDriverDetailDao).findAll(Mockito.<Specification<TiTruckDriverDetails>>any(), Mockito.<Pageable>any());
        verify(jsonHelper).convertValue(Mockito.<TiTruckDriverDetails>any(),
                Mockito.<Class<TransportInstructionLegsTruckDriverResponse>>any());
        assertEquals(1, actualListResult.getTotalPages().intValue());
        assertEquals(1, actualListResult.getTiLegsTruckDriverResponses().size());
        assertEquals(1L, actualListResult.getTotalCount().longValue());
    }

    @Test
    void testDelete() {
        Optional<TiTruckDriverDetails> emptyResult = Optional.empty();
        when(iTiTruckDriverDetailDao.findById(Mockito.<Long>any())).thenReturn(emptyResult);
        assertThrows(ValidationException.class, () -> transportInstructionLegsTruckDriverService.delete(1L));
        verify(iTiTruckDriverDetailDao).findById(Mockito.<Long>any());
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
        when(iTiTruckDriverDetailDao.findById(Mockito.<Long>any())).thenReturn(Optional.of(new TiTruckDriverDetails()));
        doNothing().when(iTiTruckDriverDetailDao).delete(any());
        transportInstructionLegsTruckDriverService.delete(1L);
        verify(iTiTruckDriverDetailDao).findById(Mockito.<Long>any());
    }


    /**
     * Method under test:
     * {@link TransportInstructionLegsContainersServiceImpl#retrieveById(Long)}
     */
    @Test
    void testRetrieveById() {
        TransportInstructionLegsTruckDriverResponse response = TransportInstructionLegsTruckDriverResponse
                .builder()
                .id(1l)
                .tiLegId(1l)
                .driverName("Rajesh")
                .driverMobileNumber("912223334445")
                .trailerNumberPlate("TRK123")
                .truckNumberPlate("truck12")
                .truckOrTrailerType("SL20").build();

        when(jsonHelper.convertValue(Mockito.<TiTruckDriverDetails>any(),
                Mockito.<Class<TransportInstructionLegsTruckDriverResponse>>any())).thenReturn(response);

        TiTruckDriverDetails truckDriverDetails = new TiTruckDriverDetails();
        Optional<TiTruckDriverDetails> ofResult = Optional.of(truckDriverDetails);
        when(iTiTruckDriverDetailDao.findById(Mockito.<Long>any())).thenReturn(ofResult);
        transportInstructionLegsTruckDriverService.retrieveById(1L);
        verify(iTiTruckDriverDetailDao).findById(Mockito.<Long>any());
        verify(jsonHelper).convertValue(Mockito.<TiTruckDriverDetails>any(),
                Mockito.<Class<TransportInstructionLegsTruckDriverResponse>>any());
    }

}
