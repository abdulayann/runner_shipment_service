package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.awb.AwbNotifyPartyInfo;
import com.dpw.runner.shipment.services.dto.request.awb.AwbShipmentInfo;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.parameters.P;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ContainersSyncTest {
    @InjectMocks
    private ContainersSync containersSync;

    @Mock
    private IContainerDao containerDao;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ModelMapper modelMapper;

    @Mock
    RestTemplate restTemplate;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private IV1Service v1Service;

    @Mock
    private EmailServiceUtility emailServiceUtility;

    @Mock
    private SyncEntityConversionService syncEntityConversionService;
    @Mock
    private ISyncService syncService;

    /**
     * Method under test: {@link ContainersSync#sync(List, Page)}
     */
    @Test
    void testSync() {
        var container1Guid = UUID.randomUUID();
        var container2Guid = UUID.randomUUID();
        // Arrange
        var mockContainer1 = new Containers();
        mockContainer1.setId(111L);
        mockContainer1.setGuid(container1Guid);
        mockContainer1.setConsolidationId(123L);

        var mockContainer2 = new Containers();
        mockContainer2.setId(122L);
        mockContainer2.setGuid(container2Guid);

        var mockShipmentContainerMapping1 = new ShipmentsContainersMapping();
        mockShipmentContainerMapping1.setContainerId(111L);
        mockShipmentContainerMapping1.setShipmentId(11L);

        var mockShipmentContainerMapping2 = new ShipmentsContainersMapping();
        mockShipmentContainerMapping2.setContainerId(111L);
        mockShipmentContainerMapping2.setShipmentId(22L);

        var mockShipment1 = new ShipmentDetails();
        mockShipment1.setId(11L);
        mockShipment1.setGuid(UUID.randomUUID());

        var mockShipment2 = new ShipmentDetails();
        mockShipment2.setId(22L);
        mockShipment2.setGuid(UUID.randomUUID());

        var mockV2Container = new ContainerRequestV2();
        mockV2Container.setGuid(container1Guid);

        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockContainer1, mockContainer2)));
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(new ConsolidationDetails()));
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockShipment1, mockShipment2)));
        doNothing().when(syncService).pushToKafka(any(), any(), any(), any(), any());
        when(syncEntityConversionService.containerV2ToV1(any())).thenReturn(mockV2Container);

        // Act
        var response = containersSync.sync(List.of(), new PageImpl<>(List.of(mockShipmentContainerMapping1, mockShipmentContainerMapping2)));

        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSync2() {
        var container1Guid = UUID.randomUUID();
        var container2Guid = UUID.randomUUID();
        // Arrange
        var mockContainer1 = new Containers();
        mockContainer1.setId(111L);
        mockContainer1.setGuid(container1Guid);
        mockContainer1.setConsolidationId(123L);

        var mockContainer2 = new Containers();
        mockContainer2.setId(122L);
        mockContainer2.setGuid(container2Guid);

        var mockShipmentContainerMapping1 = new ShipmentsContainersMapping();
        mockShipmentContainerMapping1.setContainerId(111L);
        mockShipmentContainerMapping1.setShipmentId(11L);

        var mockShipmentContainerMapping2 = new ShipmentsContainersMapping();
        mockShipmentContainerMapping2.setContainerId(111L);
        mockShipmentContainerMapping2.setShipmentId(22L);

        var mockShipment1 = new ShipmentDetails();
        mockShipment1.setId(11L);
        mockShipment1.setGuid(UUID.randomUUID());

        var mockShipment2 = new ShipmentDetails();
        mockShipment2.setId(22L);
        mockShipment2.setGuid(UUID.randomUUID());

        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockContainer1, mockContainer2)));
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(new ConsolidationDetails()));
        when(shipmentDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockShipment1, mockShipment2)));
        doNothing().when(syncService).pushToKafka(any(), any(), any(), any(), any());
        when(syncEntityConversionService.containerV2ToV1(any())).thenReturn(new ContainerRequestV2());

        // Act
        var response = containersSync.sync(List.of(), new PageImpl<>(List.of(mockShipmentContainerMapping1, mockShipmentContainerMapping2)));

        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSync3() {
        // Arrange
        var mockShipmentContainerMapping1 = new ShipmentsContainersMapping();
        mockShipmentContainerMapping1.setContainerId(111L);
        mockShipmentContainerMapping1.setShipmentId(11L);

        var mockShipmentContainerMapping2 = new ShipmentsContainersMapping();
        mockShipmentContainerMapping2.setContainerId(111L);
        mockShipmentContainerMapping2.setShipmentId(22L);


        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of()));
        doNothing().when(syncService).pushToKafka(any(), any(), any(), any(), any());

        // Act
        var response = containersSync.sync(List.of(), new PageImpl<>(List.of(mockShipmentContainerMapping1, mockShipmentContainerMapping2)));

        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSync4() {
        // Arrange
        var mockContainer1 = new Containers();
        mockContainer1.setId(111L);
        mockContainer1.setGuid(UUID.randomUUID());
        mockContainer1.setConsolidationId(123L);

        var mockContainer2 = new Containers();
        mockContainer2.setId(122L);
        mockContainer2.setGuid(UUID.randomUUID());

        var mockShipmentContainerMapping1 = new ShipmentsContainersMapping();
        mockShipmentContainerMapping1.setContainerId(111L);
        mockShipmentContainerMapping1.setShipmentId(11L);

        var mockShipmentContainerMapping2 = new ShipmentsContainersMapping();
        mockShipmentContainerMapping2.setContainerId(111L);
        mockShipmentContainerMapping2.setShipmentId(22L);

        var mockShipment1 = new ShipmentDetails();
        mockShipment1.setId(11L);
        mockShipment1.setGuid(UUID.randomUUID());

        var mockShipment2 = new ShipmentDetails();
        mockShipment2.setId(22L);
        mockShipment2.setGuid(UUID.randomUUID());

        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockContainer1, mockContainer2)));
        when(consolidationDetailsDao.findById(anyLong())).thenReturn(Optional.of(new ConsolidationDetails()));
        doNothing().when(syncService).pushToKafka(any(), any(), any(), any(), any());
        when(syncEntityConversionService.containerV2ToV1(any())).thenReturn(new ContainerRequestV2());

        // Act
        var response = containersSync.sync(List.of(), new PageImpl<>(List.of()));

        assertEquals(HttpStatus.OK, response.getStatusCode());
    }


}
