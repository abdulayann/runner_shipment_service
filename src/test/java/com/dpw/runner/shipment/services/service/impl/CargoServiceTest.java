package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.response.ContainerDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICargoService;
import com.dpw.runner.shipment.services.dto.request.ContainerDetailsRequest;
import org.junit.Before;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class CargoServiceTest {

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

    @InjectMocks
    private CargoService cargoService;

    @Test
    void testGetContainerDetailsWithBookingEntity() throws RunnerException {
        // Prepare test data
        ContainerDetailsRequest request = createRequest("BOOKING", "1");
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
        when(jsonHelper.convertValueToList(mdmResponse, MdmContainerTypeResponse.class))
                .thenReturn(Arrays.asList(mdmContainerTypeResponse));

        // Test
        ContainerDetailsResponse response = cargoService.getContainerDetails(request);

        // Assert
        assertNotNull(response);
        assertEquals(3L, response.getContainers());
        assertEquals(BigDecimal.valueOf(6), response.getTeuCount());
    }

    @Test
    void testGetContainerDetailsWithShipmentEntity() throws RunnerException {
        // Prepare test data
        ContainerDetailsRequest request = createRequest("SHIPMENT", "1");
        ShipmentDetails shipmentDetails = mock(ShipmentDetails.class);
        Containers container1 = new Containers();
        container1.setContainerCount(3L);
        container1.setContainerCode("40GP");

        // Use Set to ensure uniqueness
        Set<Containers> containers = new HashSet<>();
        containers.add(container1);

        System.out.println("Containers in Set (before JSON conversion): " + containers.size());

        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(shipmentDetails.getContainersList()).thenReturn(containers);

        DependentServiceResponse mdmResponse = mock(DependentServiceResponse.class);
        when(mdmServiceAdapter.getContainerTypes()).thenReturn(mdmResponse);
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));
        when(jsonHelper.convertValueToList(mdmResponse, MdmContainerTypeResponse.class))
                .thenReturn(Arrays.asList(mdmContainerTypeResponse));

        // Test
        ContainerDetailsResponse response = cargoService.getContainerDetails(request);

        // Assert
        assertNotNull(response);
        assertEquals(3L, response.getContainers());
        assertEquals(BigDecimal.valueOf(6), response.getTeuCount());
    }

    @Test
    void testGetContainerDetailsWithConsolidationEntity() throws RunnerException {
        // Prepare test data
        ContainerDetailsRequest request = createRequest("CONSOLIDATION", "1");
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
        when(jsonHelper.convertValueToList(mdmResponse, MdmContainerTypeResponse.class))
                .thenReturn(Arrays.asList(mdmContainerTypeResponse));

        // Test
        ContainerDetailsResponse response = cargoService.getContainerDetails(request);

        // Assert
        assertNotNull(response);
        assertEquals(11L, response.getContainers());
        assertEquals(BigDecimal.valueOf(33), response.getTeuCount());
    }

    @Test
    void testGetContainerDetailsWithUnknownEntityType() throws RunnerException {
        // Prepare test data
        ContainerDetailsRequest request = createRequest("UNKNOWN", "1");

        // Test
        ContainerDetailsResponse response = cargoService.getContainerDetails(request);

        // Assert
        assertNotNull(response);
        assertEquals(0L, response.getContainers()); // Total containers should be 0
        assertEquals(BigDecimal.ZERO, response.getTeuCount()); // TEU count should be 0
    }

    @Test
    void testGetContainerDetailsWithBookingEntityNoContainers() throws RunnerException {
        // Prepare test data
        ContainerDetailsRequest request = createRequest("BOOKING", "1");
        CustomerBooking customerBooking = mock(CustomerBooking.class);
        when(customerBookingDao.findById(1L)).thenReturn(Optional.of(customerBooking));
        when(customerBooking.getContainersList()).thenReturn(Collections.emptyList());
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("20GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));

        // Test
        ContainerDetailsResponse response = cargoService.getContainerDetails(request);

        // Assert
        assertNotNull(response);
        assertEquals(0L, response.getContainers());
        assertEquals(BigDecimal.ZERO, response.getTeuCount());
    }

    @Test
    void testGetContainerDetailsWithShipmentEntityNoContainers() throws RunnerException {
        // Prepare test data
        ContainerDetailsRequest request = createRequest("SHIPMENT", "1");
        ShipmentDetails shipmentDetails = mock(ShipmentDetails.class);
        when(shipmentDao.findById(1L)).thenReturn(Optional.of(shipmentDetails));
        when(shipmentDetails.getContainersList()).thenReturn(new HashSet<>());
        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40GP");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(2));

        // Test
        ContainerDetailsResponse response = cargoService.getContainerDetails(request);

        // Assert
        assertNotNull(response);
        assertEquals(0L, response.getContainers());
        assertEquals(BigDecimal.ZERO, response.getTeuCount());
    }

    @Test
    void testGetContainerDetailsWithConsolidationEntityNoContainers() throws RunnerException {
        // Prepare test data
        ContainerDetailsRequest request = createRequest("CONSOLIDATION", "1");
        ConsolidationDetails consolidationDetails = mock(ConsolidationDetails.class);
        when(consolidationDetailsDao.findById(1L)).thenReturn(Optional.of(consolidationDetails));
        when(consolidationDetails.getContainersList()).thenReturn(Collections.emptyList());

        MdmContainerTypeResponse mdmContainerTypeResponse = new MdmContainerTypeResponse();
        mdmContainerTypeResponse.setCode("40HQ");
        mdmContainerTypeResponse.setTeu(BigDecimal.valueOf(3));

        // Test
        ContainerDetailsResponse response = cargoService.getContainerDetails(request);

        // Assert
        assertNotNull(response);
        assertEquals(0L, response.getContainers()); // No containers
        assertEquals(BigDecimal.ZERO, response.getTeuCount()); // TEU count should be 0
    }


    private ContainerDetailsRequest createRequest(String entityType, String entityId) {
        ContainerDetailsRequest request = new ContainerDetailsRequest();
        request.setEntityType(entityType);
        request.setEntityId(entityId);
        return request;
    }

}
