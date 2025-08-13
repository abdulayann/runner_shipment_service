package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.CargoChargeableRequest;
import com.dpw.runner.shipment.services.dto.request.CargoDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.CargoChargeableResponse;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentSummaryWarningsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingV3Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.v3.ShipmentsV3Util;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CargoServiceTest {

    @Mock
    private IMDMServiceAdapter mdmServiceAdapter;

    @Mock
    private ICustomerBookingDao customerBookingDao;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ConsolidationV3Service consolidationService;

    @Mock
    private CustomerBookingV3Service customerBookingV3Service;

    @Mock
    private ShipmentsV3Util shipmentsV3Util;

    @Mock
    private CommonUtils commonUtils;

    @InjectMocks
    private CargoService cargoService;

    @Test
    void testGetCargoDetailsWithBookingEntity() throws RunnerException {
        CargoDetailsRequest request = createRequest("BOOKING", "1");
        CustomerBooking customerBooking = new CustomerBooking();
        customerBooking.setContainers(3L);
        customerBooking.setPackingList(new ArrayList<>());
        customerBooking.setTeuCount(new BigDecimal("6.0"));
        Containers container1 = new Containers();
        container1.setContainerCount(1L);
        container1.setContainerCode("20GP");
        container1.setPackagesPerContainer(2L);
        Containers container2 = new Containers();
        container2.setContainerCount(2L);
        container1.setPackagesPerContainer(2L);
        container2.setContainerCode("20GP");
        customerBooking.setContainersList(List.of(container1, container2));
        when(customerBookingV3Service.getTotalCargoWeight(any(), any())).thenReturn(BigDecimal.ZERO);
        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(customerBooking));
        // Test
        CargoDetailsResponse response = cargoService.getCargoDetails(request);
        // Assert
        assertNotNull(response);
        assertEquals(3, response.getContainers());
        assertEquals(BigDecimal.valueOf(6.0), response.getTeuCount());
    }

    @Test
    void testGetContainerDetailsWithBookingEntityNoContainers() throws RunnerException {
        // Prepare test data
        CargoDetailsRequest request = createRequest("BOOKING", "1");
        CustomerBooking customerBooking = mock(CustomerBooking.class);
        when(customerBookingV3Service.getTotalCargoWeight(any(), any())).thenReturn(BigDecimal.ZERO);
        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(customerBooking));

        // Test
        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        // Assert
        assertNotNull(response);
    }

    @Test
    void testGetCargoDetailsForCustomerBooking_WithContainersAndPackings() throws RunnerException {
        CargoDetailsRequest request = createRequest("BOOKING", "1");

        CustomerBooking booking = new CustomerBooking();
        booking.setTransportType("AIR");
        booking.setCargoType("LCL");
        booking.setContainers(2L);
        Containers container = new Containers();
        container.setContainerCode("20GP");
        container.setContainerCount(2L);
        booking.setContainersList(List.of(container));

        Packing packing = new Packing();
        packing.setWeight(BigDecimal.valueOf(100));
        packing.setWeightUnit("KG");
        packing.setVolume(BigDecimal.valueOf(2));
        packing.setVolumeUnit("M3");
        packing.setPacks("10");
        packing.setPacksType("PKG");
        booking.setPackingList(List.of(packing));

        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(booking));
        when(customerBookingV3Service.getTotalContainerPackages(List.of(container))).thenReturn(10L);
        when(customerBookingV3Service.getTotalCargoWeight(anyList(), any())).thenReturn(BigDecimal.TEN);

        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        assertEquals(2, response.getContainers());
    }

    @Test
    void testGetCargoDetailsForCustomerBooking_WithContainersAndPackings_ForSeaTransport() throws RunnerException {
        CargoDetailsRequest request = createRequest("BOOKING", "1");

        CustomerBooking booking = new CustomerBooking();
        booking.setTransportType("AIR");
        booking.setCargoType("LCL");
        booking.setContainers(2L);
        Containers container = new Containers();
        container.setContainerCode("20GP");
        container.setContainerCount(2L);
        booking.setContainersList(List.of(container));
        booking.setTransportType("SEA");

        Packing packing = new Packing();
        packing.setWeight(BigDecimal.valueOf(100));
        packing.setWeightUnit("KG");
        packing.setVolume(BigDecimal.valueOf(2));
        packing.setVolumeUnit("M3");
        packing.setPacks("10");
        packing.setPacksType("PKG");
        booking.setPackingList(List.of(packing));
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("20GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(List.of(mdmContainerTypeResponse));

        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(booking));
        when(customerBookingV3Service.getTotalContainerPackages(List.of(container))).thenReturn(10L);
        when(customerBookingV3Service.getTotalCargoWeight(anyList(), any())).thenReturn(BigDecimal.TEN);

        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        assertEquals(2, response.getContainers());
    }

    @Test
    void testGetCargoDetailsForCustomerBooking_WithContainersAndPackings_WithNullSeaTransport() throws RunnerException {
        CargoDetailsRequest request = createRequest("BOOKING", "1");

        CustomerBooking booking = new CustomerBooking();
        booking.setTransportType("AIR");
        booking.setCargoType("LCL");
        booking.setContainers(2L);
        Containers container = new Containers();
        container.setContainerCode("20GP");
        container.setContainerCount(2L);
        booking.setContainersList(List.of(container));
        booking.setTransportType(null);

        Packing packing = new Packing();
        packing.setPacks("2");
        packing.setPacksType("PKG");
        packing.setWeight(BigDecimal.valueOf(100));
        packing.setWeightUnit("KG");
        packing.setVolume(BigDecimal.valueOf(2));
        packing.setVolumeUnit("M3");
        packing.setPacks("10");
        booking.setPackingList(List.of(packing));
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("20GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(List.of(mdmContainerTypeResponse));

        when(customerBookingV3Service.getTotalContainerPackages(List.of(container))).thenReturn(10L);
        when(customerBookingV3Service.getTotalCargoWeight(anyList(), any())).thenReturn(BigDecimal.TEN);
        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(booking));

        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        assertEquals(2, response.getContainers());
    }

    @Test
    void testGetCargoDetailsForCustomerBooking_MissingWeightForAir() throws RunnerException {
        CargoDetailsRequest request = createRequest("BOOKING", "1");

        CustomerBooking booking = new CustomerBooking();
        booking.setTransportType("AIR");
        booking.setCargoType("LCL");
        booking.setContainers(1L);
        booking.setTeuCount(new BigDecimal("2.0"));
        booking.setGrossWeight(BigDecimal.ZERO);

        Containers container = new Containers();
        container.setContainerCode("20GP");
        container.setContainerCount(1L);
        booking.setContainersList(List.of(container));

        Packing packing = new Packing();
        packing.setWeight(null);
        packing.setPacks("2");
        packing.setPacksType("PKG");
        packing.setWeightUnit("KG");
        packing.setVolume(BigDecimal.valueOf(3));
        packing.setVolumeUnit("M3");
        packing.setPacks("5");
        booking.setPackingList(List.of(packing));

        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("20GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(List.of(mdmContainerTypeResponse));

        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(booking));
        when(customerBookingV3Service.getTotalContainerPackages(anyList())).thenReturn(10L);
        when(customerBookingV3Service.getTotalCargoWeight(anyList(), any())).thenReturn(BigDecimal.TEN);

        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        assertEquals(1, response.getContainers());
    }

    @Test
    void testCalculateChargeable_ForAirTransport_ShouldRoundOff() throws RunnerException {
        CargoChargeableRequest request = new CargoChargeableRequest();
        request.setTransportMode("AIR");
        request.setWeightUnit("KG");
        request.setVolumeUnit("M3");
        request.setWeight(new BigDecimal("120.4"));
        request.setVolume(new BigDecimal("2.5"));

        VolumeWeightChargeable vwMock = new VolumeWeightChargeable();
        vwMock.setChargeable(new BigDecimal("123.3")); // will be rounded to 123.5
        vwMock.setChargeableUnit("KG");
        vwMock.setVolumeWeight(new BigDecimal("100.0"));
        vwMock.setVolumeWeightUnit("KG");

        Mockito.when(consolidationService.calculateVolumeWeight(
                ("AIR"), ("KG"), ("M3"),
                (new BigDecimal("120.4")), (new BigDecimal("2.5"))
        )).thenReturn(vwMock);

        CargoChargeableResponse response = cargoService.calculateChargeable(request);

        assertNotNull(response);
        assertEquals(new BigDecimal("120.4"), response.getWeight());
    }

    @Test
    void testCalculateChargeable_ForSeaTransport_NoRounding() throws RunnerException {
        CargoChargeableRequest request = new CargoChargeableRequest();
        request.setTransportMode("SEA");
        request.setWeightUnit("KG");
        request.setVolumeUnit("M3");
        request.setWeight(new BigDecimal("500.0"));
        request.setVolume(new BigDecimal("10.0"));

        VolumeWeightChargeable vwMock = new VolumeWeightChargeable();
        vwMock.setChargeable(new BigDecimal("510.75"));
        vwMock.setChargeableUnit("KG");
        vwMock.setVolumeWeight(new BigDecimal("120.0"));
        vwMock.setVolumeWeightUnit("KG");

        Mockito.when(consolidationService.calculateVolumeWeight(
                ("SEA"), ("KG"), ("M3"),
                (new BigDecimal("500.0")), (new BigDecimal("10.0"))
        )).thenReturn(vwMock);

        CargoChargeableResponse response = cargoService.calculateChargeable(request);

        assertNotNull(response);
        assertEquals(new BigDecimal("500.0"), response.getWeight());
    }


    private CargoDetailsRequest createRequest(String entityType, String entityId) {
        CargoDetailsRequest request = new CargoDetailsRequest();
        request.setEntityType(entityType);
        request.setEntityId(entityId);
        return request;
    }

}
