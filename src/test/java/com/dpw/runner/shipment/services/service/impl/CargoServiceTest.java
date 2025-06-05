package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.CargoDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
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
    private IConsolidationService consolidationService;

    @InjectMocks
    private CargoService cargoService;

    @Test
    void testGetCargoDetailsWithBookingEntity() throws RunnerException {
        CargoDetailsRequest request = createRequest("BOOKING", "1");
        CustomerBooking customerBooking = mock(CustomerBooking.class);
        Containers container1 = new Containers();
        container1.setContainerCount(1L);
        container1.setContainerCode("20GP");
        Containers container2 = new Containers();
        container2.setContainerCount(2L);
        container2.setContainerCode("20GP");
        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(customerBooking));
        when(customerBooking.getContainersList()).thenReturn(Arrays.asList(container1, container2));

        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);

        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("20GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));

        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "20GP")));
        when(jsonHelper.convertToJson(mdmResponse.getData())).thenReturn("dummyJson");
        when(jsonHelper.convertJsonToMap("dummyJson")).thenReturn(
                Map.of("data", Arrays.asList(Collections.singletonMap("code", "20GP")))
        );
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        // Test
        CargoDetailsResponse response = cargoService.getCargoDetails(request);
        // Assert
        assertNotNull(response);
        assertEquals(3, response.getContainers());
        assertEquals(BigDecimal.valueOf(6.0), response.getTeuCount());
    }

    @Test
    void testGetCargoDetailsWithShipmentEntity() throws RunnerException {
        CargoDetailsRequest request = createRequest("SHIPMENT", "1");
        ShipmentDetails shipmentDetails = mock(ShipmentDetails.class);
        Containers container1 = new Containers();
        container1.setContainerCount(3L);
        container1.setContainerCode("40GP");

        // Use Set to ensure uniqueness
        Set<Containers> containers = new HashSet<>();
        containers.add(container1);

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(shipmentDetails.getContainersList()).thenReturn(containers);

        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertToJson(mdmResponse.getData())).thenReturn("dummyJson");
        when(jsonHelper.convertJsonToMap("dummyJson")).thenReturn(
                Map.of("data", Arrays.asList(Collections.singletonMap("code", "40GP")))
        );
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));
        // Test
        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        // Assert
        assertNotNull(response);
        assertEquals(3, response.getContainers());
        assertEquals(BigDecimal.valueOf(6.0), response.getTeuCount());
    }

    @Test
    void testGetCargoDetailsWithConsolidationEntity() throws RunnerException {
        CargoDetailsRequest request = createRequest("CONSOLIDATION", "1");
        ConsolidationDetails consolidationDetails = mock(ConsolidationDetails.class);
        Containers container1 = new Containers();
        container1.setContainerCount(5L);
        container1.setContainerCode("40HQ");
        Containers container2 = new Containers();
        container2.setContainerCount(6L);
        container2.setContainerCode("40HQ");
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(consolidationDetails.getContainersList()).thenReturn(Arrays.asList(container1, container2));

        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40HQ");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(3));
        Map<String, Object> mdmMap = new HashMap<>();
        mdmMap.put("data", Arrays.asList(Collections.singletonMap("code", "40GP")));
        when(jsonHelper.convertToJson(mdmResponse.getData())).thenReturn("dummyJson");
        when(jsonHelper.convertJsonToMap("dummyJson")).thenReturn(
                Map.of("data", Arrays.asList(Collections.singletonMap("code", "40GP")))
        );
        when(jsonHelper.convertValueToList(any(), any())).thenReturn(Arrays.asList(mdmContainerTypeResponse));

        // Test
        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        // Assert
        assertNotNull(response);
        assertEquals(11, response.getContainers());
        assertEquals(BigDecimal.valueOf(33.0), response.getTeuCount());
    }

    @Test
    void testGetCargoDetailsWithUnknownEntityType() throws RunnerException {
        CargoDetailsRequest request = createRequest("UNKNOWN", "1");

        // Test
        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        // Assert
        assertNotNull(response);
        assertNull(response.getContainers());
        assertNull(response.getTeuCount());
    }

    @Test
    void testGetContainerDetailsWithBookingEntityNoContainers() throws RunnerException {
        // Prepare test data
        CargoDetailsRequest request = createRequest("BOOKING", "1");
        CustomerBooking customerBooking = mock(CustomerBooking.class);
        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(customerBooking));
        when(customerBooking.getContainersList()).thenReturn(Collections.emptyList());
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("20GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));

        // Test
        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        // Assert
        assertNotNull(response);
        assertNull(response.getContainers());
        assertNull(response.getTeuCount());
    }

    @Test
    void testGetContainerDetailsWithShipmentEntityNoContainers() throws RunnerException {
        CargoDetailsRequest request = createRequest("SHIPMENT", "1");
        ShipmentDetails shipmentDetails = mock(ShipmentDetails.class);
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(shipmentDetails.getContainersList()).thenReturn(new HashSet<>());
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));

        // Test
        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        // Assert
        assertNotNull(response);
        assertNull(response.getContainers());
        assertNull(response.getTeuCount());
    }

    @Test
    void testGetCargoDetailsWithConsolidationEntityNoContainers() throws RunnerException {
        CargoDetailsRequest request = createRequest("CONSOLIDATION", "1");
        ConsolidationDetails consolidationDetails = mock(ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(consolidationDetails.getContainersList()).thenReturn(Collections.emptyList());

        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40HQ");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(3));

        // Test
        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        // Assert
        assertNotNull(response);
        assertNull(response.getContainers());
        assertNull(response.getTeuCount());
    }

    @Test
    void testGetCargoDetailsForCustomerBooking_WithContainersAndPackings() throws RunnerException {
        CargoDetailsRequest request = createRequest("BOOKING", "1");

        CustomerBooking booking = new CustomerBooking();
        booking.setTransportType("AIR");
        booking.setCargoType("LCL");
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
        booking.setPackingList(List.of(packing));
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("20GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(List.of(mdmContainerTypeResponse));

        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(booking));
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertJsonToMap(any())).thenReturn(Map.of("data", List.of(mdmContainerTypeResponse)));
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(jsonHelper.convertValueToList(any(), eq(MdmContainerTypeResponse.class))).thenReturn(List.of(mdmContainerTypeResponse));

        VolumeWeightChargeable vwOb = new VolumeWeightChargeable();
        vwOb.setChargeable(BigDecimal.valueOf(100));
        vwOb.setChargeableUnit("KG");
        vwOb.setVolumeWeight(BigDecimal.valueOf(150));
        vwOb.setVolumeWeightUnit("KG");
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(vwOb);

        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        assertEquals(2, response.getContainers());
        assertEquals(BigDecimal.valueOf(4.0), response.getTeuCount());
        assertEquals(BigDecimal.valueOf(100.0), response.getWeight());
        assertEquals(BigDecimal.valueOf(2.0), response.getVolume());
        assertEquals(10, response.getNoOfPacks());
        assertEquals(BigDecimal.valueOf(100.0), response.getChargable());
        assertEquals(BigDecimal.valueOf(150), response.getVolumetricWeight());
        assertEquals("KG", response.getVolumetricWeightUnit());
    }

    @Test
    void testGetCargoDetailsForCustomerBooking_WithContainersAndPackings_ForSeaTransport() throws RunnerException {
        CargoDetailsRequest request = createRequest("BOOKING", "1");

        CustomerBooking booking = new CustomerBooking();
        booking.setTransportType("AIR");
        booking.setCargoType("LCL");
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
        booking.setPackingList(List.of(packing));
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("20GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(List.of(mdmContainerTypeResponse));

        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(booking));
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertJsonToMap(any())).thenReturn(Map.of("data", List.of(mdmContainerTypeResponse)));
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(jsonHelper.convertValueToList(any(), eq(MdmContainerTypeResponse.class))).thenReturn(List.of(mdmContainerTypeResponse));

        VolumeWeightChargeable vwOb = new VolumeWeightChargeable();
        vwOb.setChargeable(BigDecimal.valueOf(100));
        vwOb.setChargeableUnit("KG");
        vwOb.setVolumeWeight(BigDecimal.valueOf(150));
        vwOb.setVolumeWeightUnit("KG");
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(vwOb);

        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        assertEquals(2, response.getContainers());
        assertEquals(BigDecimal.valueOf(4.0), response.getTeuCount());
        assertEquals(BigDecimal.valueOf(100.0), response.getWeight());
        assertEquals(BigDecimal.valueOf(2.0), response.getVolume());
        assertEquals(10, response.getNoOfPacks());
        assertEquals(BigDecimal.valueOf(2.0), response.getChargable());
        assertEquals(BigDecimal.valueOf(150), response.getVolumetricWeight());
        assertEquals("KG", response.getVolumetricWeightUnit());
    }

    @Test
    void testGetCargoDetailsForCustomerBooking_WithContainersAndPackings_WithNullSeaTransport() throws RunnerException {
        CargoDetailsRequest request = createRequest("BOOKING", "1");

        CustomerBooking booking = new CustomerBooking();
        booking.setTransportType("AIR");
        booking.setCargoType("LCL");
        Containers container = new Containers();
        container.setContainerCode("20GP");
        container.setContainerCount(2L);
        booking.setContainersList(List.of(container));
        booking.setTransportType(null);

        Packing packing = new Packing();
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

        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(booking));
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertJsonToMap(any())).thenReturn(Map.of("data", List.of(mdmContainerTypeResponse)));
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(jsonHelper.convertValueToList(any(), eq(MdmContainerTypeResponse.class))).thenReturn(List.of(mdmContainerTypeResponse));

        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        assertEquals(2, response.getContainers());
        assertEquals(BigDecimal.valueOf(4.0), response.getTeuCount());
        assertEquals(BigDecimal.valueOf(100.0), response.getWeight());
        assertEquals(BigDecimal.valueOf(2.0), response.getVolume());
        assertEquals(10, response.getNoOfPacks());
    }

    @Test
    void testGetCargoDetailsForShipment_WithContainersAndPackings() throws RunnerException {
        CargoDetailsRequest request = createRequest("SHIPMENT", "1");

        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setTransportMode("AIR");
        shipmentDetails.setShipmentType("LCL");
        Containers container = new Containers();
        container.setContainerCode("20GP");
        container.setContainerCount(2L);
        shipmentDetails.setContainersList(Set.of(container));

        Packing packing = new Packing();
        packing.setWeight(BigDecimal.valueOf(100));
        packing.setWeightUnit("KG");
        packing.setVolume(BigDecimal.valueOf(2));
        packing.setVolumeUnit("M3");
        packing.setPacks("10");
        shipmentDetails.setPackingList(List.of(packing));
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("20GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(List.of(mdmContainerTypeResponse));

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertJsonToMap(any())).thenReturn(Map.of("data", List.of(mdmContainerTypeResponse)));
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(jsonHelper.convertValueToList(any(), eq(MdmContainerTypeResponse.class))).thenReturn(List.of(mdmContainerTypeResponse));

        VolumeWeightChargeable vwOb = new VolumeWeightChargeable();
        vwOb.setChargeable(BigDecimal.valueOf(100));
        vwOb.setChargeableUnit("KG");
        vwOb.setVolumeWeight(BigDecimal.valueOf(150));
        vwOb.setVolumeWeightUnit("KG");
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(vwOb);

        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        assertEquals(2, response.getContainers());
        assertEquals(BigDecimal.valueOf(4.0), response.getTeuCount());
        assertEquals(BigDecimal.valueOf(100.0), response.getWeight());
        assertEquals(BigDecimal.valueOf(2.0), response.getVolume());
        assertEquals(10, response.getNoOfPacks());
        assertEquals(BigDecimal.valueOf(100.0), response.getChargable());
        assertEquals(BigDecimal.valueOf(150), response.getVolumetricWeight());
        assertEquals("KG", response.getVolumetricWeightUnit());
    }

    @Test
    void testGetCargoDetailsForConsolidation_WithContainersAndPackings() throws RunnerException {
        CargoDetailsRequest request = createRequest("CONSOLIDATION", "1");

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        consolidationDetails.setTransportMode("AIR");
        consolidationDetails.setShipmentType("LCL");
        Containers container = new Containers();
        container.setContainerCode("20GP");
        container.setContainerCount(2L);
        consolidationDetails.setContainersList(List.of(container));

        Packing packing = new Packing();
        packing.setWeight(BigDecimal.valueOf(100));
        packing.setWeightUnit("KG");
        packing.setVolume(BigDecimal.valueOf(2));
        packing.setVolumeUnit("M3");
        packing.setPacks("10");
        consolidationDetails.setPackingList(List.of(packing));
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("20GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(List.of(mdmContainerTypeResponse));

        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertJsonToMap(any())).thenReturn(Map.of("data", List.of(mdmContainerTypeResponse)));
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(jsonHelper.convertValueToList(any(), eq(MdmContainerTypeResponse.class))).thenReturn(List.of(mdmContainerTypeResponse));

        VolumeWeightChargeable vwOb = new VolumeWeightChargeable();
        vwOb.setChargeable(BigDecimal.valueOf(100));
        vwOb.setChargeableUnit("KG");
        vwOb.setVolumeWeight(BigDecimal.valueOf(150));
        vwOb.setVolumeWeightUnit("KG");
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(vwOb);

        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        assertEquals(2, response.getContainers());
        assertEquals(BigDecimal.valueOf(4.0), response.getTeuCount());
        assertEquals(BigDecimal.valueOf(100.0), response.getWeight());
        assertEquals(BigDecimal.valueOf(2.0), response.getVolume());
        assertEquals(10, response.getNoOfPacks());
        assertEquals(BigDecimal.valueOf(100.0), response.getChargable());
        assertEquals(BigDecimal.valueOf(150), response.getVolumetricWeight());
        assertEquals("KG", response.getVolumetricWeightUnit());
    }

    @Test
    void testGetCargoDetailsForCustomerBooking_MissingWeightForAir() throws RunnerException {
        CargoDetailsRequest request = createRequest("BOOKING", "1");

        CustomerBooking booking = new CustomerBooking();
        booking.setTransportType("AIR");
        booking.setCargoType("LCL");

        Containers container = new Containers();
        container.setContainerCode("20GP");
        container.setContainerCount(1L);
        booking.setContainersList(List.of(container));

        Packing packing = new Packing();
        packing.setWeight(null);
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
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(dependentServiceResponse);
        when(jsonHelper.convertJsonToMap(any())).thenReturn(Map.of("data", List.of(mdmContainerTypeResponse)));
        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(jsonHelper.convertValueToList(any(), eq(MdmContainerTypeResponse.class))).thenReturn(List.of(mdmContainerTypeResponse));

        VolumeWeightChargeable vwOb = new VolumeWeightChargeable();
        vwOb.setChargeable(null);
        vwOb.setChargeableUnit("KG");
        vwOb.setVolumeWeight(BigDecimal.valueOf(480));
        vwOb.setVolumeWeightUnit("KG");
        when(consolidationService.calculateVolumeWeight(any(), any(), any(), any(), any())).thenReturn(vwOb);

        CargoDetailsResponse response = cargoService.getCargoDetails(request);

        assertEquals(1, response.getContainers());
        assertEquals(BigDecimal.valueOf(2.0), response.getTeuCount());
        assertNull(response.getWeight());
        assertNull(response.getChargable());
        assertEquals(BigDecimal.valueOf(3.0), response.getVolume());
        assertEquals(5, response.getNoOfPacks());
        assertEquals(BigDecimal.valueOf(480), response.getVolumetricWeight());
        assertEquals("KG", response.getVolumetricWeightUnit());
    }


    private CargoDetailsRequest createRequest(String entityType, String entityId) {
        CargoDetailsRequest request = new CargoDetailsRequest();
        request.setEntityType(entityType);
        request.setEntityId(entityId);
        return request;
    }

}
